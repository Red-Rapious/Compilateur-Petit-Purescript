open Ast
open Pretty

let rec typ_eq t1 t2 =
  match (t1, t2) with
  | TUnit, TUnit | TStr, TStr | TInt, TInt | TBool, TBool -> true
  | TEffect t1, TEffect t2 -> typ_eq t1 t2
  | TArrow (t1, t2), TArrow (t3, t4) ->
      List.for_all2 typ_eq t1 t3 && typ_eq t2 t4
  | TVar t1, TVar t2 -> (
      match (t1.def, t2.def) with
      | None, None -> true
      | Some t1, Some t2 -> typ_eq t1 t2
      | _, _ -> false)
  | _, _ -> false

module Smapi = Map.Make (Int)

type envi = ttyp Smapi.t

module V = struct
  type t = tvar

  let compare v1 v2 = Stdlib.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id

  let create =
    let r = ref 0 in
    fun () ->
      incr r;
      { id = !r; def = None }
end

let rec head = function TVar { def = Some t } -> head t | t -> t

let rec canon t =
  match head t with
  | (TInt | TBool | TStr | TUnit) as t -> t
  | TEffect t1 -> TEffect (canon t1)
  | TVar { id = a; def = None } as t -> t
  | TVar { id = a; def = Some t } -> TVar { id = a; def = Some (canon t) }
  | TArrow (t1, t2) -> TArrow (List.map canon t1, canon t2)

exception UnificationFailure of ttyp * ttyp

let unification_error t1 t2 = raise (UnificationFailure (canon t1, canon t2))

exception Typing_error of loc * string

let typing_error loc s = raise (Typing_error (loc, s))

exception Empty_pattern_matching of loc
exception Unknown_ident of loc * Ast.ident

let rec occur v t =
  match head t with
  | TVar w -> V.equal v w
  | TArrow (t1, t2) -> List.exists (fun e -> occur v e) t1 || occur v t2
  | _ -> false

let rec unify t1 t2 =
  match (head t1, head t2) with
  | TVar v1, TVar v2 when V.equal v1 v2 -> ()
  | (TVar v1 as t1), t2 ->
      if occur v1 t2 then unification_error t1 t2;
      assert (v1.def = None);
      v1.def <- Some t2
  | t1, TVar v2 -> unify t2 t1
  | TArrow (t11, t12), TArrow (t21, t22) ->
      List.iter2 unify t11 t21;
      unify t12 t22
  | TInt, TInt | TBool, TBool | TStr, TStr | TUnit, TUnit -> ()
  | TEffect t1, TEffect t2 when typ_eq t1 t2 -> ()
  | t1, t2 -> unification_error t1 t2

module Vset = Set.Make (V)

let rec fvars t =
  match head t with
  | TInt | TStr | TBool | TUnit -> Vset.empty
  | TEffect t -> fvars t
  | TArrow (t1, t2) -> List.fold_left Vset.union (fvars t2) (List.map fvars t1)
  | TVar v -> Vset.singleton v

let norm_varset s =
  Vset.fold (fun v s -> Vset.union (fvars (TVar v)) s) s Vset.empty

type schema = { vars : Vset.t; ttyp : ttyp }

module Smaps = Map.Make (String)

type envs = { bindings : schema Smaps.t; fvars : Vset.t }

let empty = { bindings = Smaps.empty; fvars = Vset.empty }

let add gen x t global_env =
  let vt = fvars t in
  let s, fvars =
    if gen then
      let env_fvars = norm_varset global_env.fvars in
      ({ vars = Vset.diff vt env_fvars; ttyp = t }, global_env.fvars)
    else ({ vars = Vset.empty; ttyp = t }, Vset.union global_env.fvars vt)
  in
  { bindings = Smaps.add x s global_env.bindings; fvars }

module Vmap = Map.Make (V)

let find x global_env =
  let tx = Smaps.find x global_env.bindings in
  let s =
    Vset.fold (fun v s -> Vmap.add v (TVar (V.create ())) s) tx.vars Vmap.empty
  in
  let rec subst t =
    match head t with
    | TVar x as t -> ( try Vmap.find x s with Not_found -> t)
    | (TInt | TBool | TStr | TUnit) as t -> t
    | TEffect t -> TEffect (subst t)
    | TArrow (t1, t2) -> TArrow (List.map subst t1, subst t2)
  in
  subst tx.ttyp

let rec ast_type global_env local_env = function
  | Tatype a -> ast_atype global_env local_env a
  | Tntype n -> ast_ntype global_env local_env n

and ast_atype global_env local_env = function
  | Tident "String" -> TStr
  | Tident "Int" -> TInt
  | Tident "Boolean" -> TBool
  | Tident "Unit" -> TUnit
  | Ttype t -> ast_type global_env local_env t
  | _ -> TBool

and ast_ntype global_env local_env = function
  (*Pakomprienfét*)
  | id, h :: t -> TInt
  | id, [] -> TBool

let rec typ_exp global_env local_env loc_expr =
  let loc, expr = loc_expr in
  match expr with
  | Eunop (_, e) -> (
      match typ_exp global_env local_env e with
      | TInt -> TInt
      | _ ->
          typing_error (fst e)
            "mauvais opérande pour l'opérateur unaire '-' : le type attendu \
             est Int")
  | Eif (e1, e2, e3) -> (
      match typ_exp global_env local_env e1 with
      | TBool ->
          let s2 = typ_exp global_env local_env e2 in
          let s3 = typ_exp global_env local_env e3 in
          if s2 == s3 then s2
          else
            typing_error (fst e3) "les deux branches doivent être de même type"
      | _ ->
          typing_error (fst e1)
            "mauvais type de l'expression de condition : le type attendu est \
             Bool")
  | Ebinop (e1, op, e2) -> (
      let s1 = typ_exp global_env local_env e1 in
      let s2 = typ_exp global_env local_env e2 in
      let binop_string = Pretty.print_binop op in
      if s1 <> s2 then
        typing_error (fst e2)
          ("mauvais opérande pour '" ^ binop_string
         ^ "' : les deux types doivent être identiques");
      match op with
      | Beq | Bneq -> (
          match s1 with
          | TInt | TBool | TStr -> TBool
          | _ ->
              typing_error (fst e1)
                ("mauvais opérande pour l'opérateur '" ^ binop_string
               ^ "' : les expressions comparées doivent être de type Int, \
                  String ou Bool"))
      | Blt | Ble | Bgt | Bge -> (
          match s1 with
          | TInt -> TBool
          | _ ->
              typing_error (fst e1)
                ("mauvais opérande pour l'opérateur '" ^ binop_string
               ^ "' : les expressions comparées doivent être de type Int"))
      | Bsub | Badd | Bmul | Bdiv -> (
          match s1 with
          | TInt -> TInt
          | _ ->
              typing_error (fst e1)
                ("mauvais opérande pour l'opérateur '" ^ binop_string
               ^ "' : les expressions manipulées doivent être de type Int"))
      | Bconcat -> (
          match s1 with
          | TStr -> TStr
          | _ ->
              typing_error (fst e1)
                ("mauvais opérande pour l'opérateur '" ^ binop_string
               ^ "' : les expressions concaténées doivent être de type Str"))
      | Band | Bor -> (
          match s1 with
          | TBool -> TBool
          | _ ->
              typing_error (fst e1)
                ("mauvais opérande pour l'opérateur '" ^ binop_string
               ^ "' : les expressions manipulées doivent être de type Bool")))
  | Eatom a -> typ_atom (loc, a)
  | Edo l ->
      List.iter
        (fun e ->
          if not (typ_eq (typ_exp global_env local_env e) (TEffect TUnit)) then
            typing_error (fst e) "le type attendu est Unit")
        l;
      TEffect TUnit
  | Ecase (e, []) -> raise (Empty_pattern_matching (fst e))
  | Ecase (e, l) ->
      let t = typ_exp global_env local_env e in
      let rettypes = List.map (fun x -> typ_branch global_env local_env x) l in
      let tret = snd (List.hd rettypes) in
      (* TODO: préciser encore plus quelle branche est mal typée *)
      List.iter
        (fun x ->
          if not (typ_eq t (fst x) && typ_eq tret (snd x)) then
            typing_error (fst e)
              "l'expression n'est pas du même type que le pattern de la branche"
            (*Ce message d'erreur ne veut rien dire*))
        rettypes;
      tret
  | Elet (bl, e) ->
      typ_exp
        (List.fold_left (fun global_env x -> typ_binding global_env local_env x) global_env bl)
        local_env e
  | _ -> TInt

and typ_atom global_env ntypes (l, a) =
  match a with
  | Aconst c -> (
      match c with Cbool _ -> TBool | Cint _ -> TInt | Cstring _ -> TStr)
  | Aident id -> (
      try find id global_env with Not_found -> raise (Unknown_ident (l, id)))
  | Aexpr e -> typ_exp global_env ntypes e
  | Atypedexpr (e, tau) ->
      let t = ast_type global_env ntypes tau in
      if typ_eq t (typ_exp global_env ntypes e) then t
        (* TODO: spécifier les deux types *)
      else typing_error (fst e) "l'expression n'est pas du type spécifié."

and typ_pattern global_env ntypes = function
  | Parg (l, p) -> typ_patarg global_env ntypes (l, p)
  | Pnamedarg (id, l) -> TBool (*Pas compris encore*)

and typ_patarg global_env ntypes (l, p) =
  match p with
  | Pconst c -> (
      match c with Cbool _ -> TBool | Cint _ -> TInt | Cstring _ -> TStr)
  | Pident id -> (
      try find id global_env with Not_found -> raise (Unknown_ident (l, id)))
  | Ppattern p -> typ_pattern global_env ntypes p

and typ_binding global_env local_env = function
  | id, e -> add true id (typ_exp global_env local_env e) global_env

and typ_branch global_env ntypes (p, e) =
  (typ_pattern global_env ntypes p, typ_exp global_env ntypes e)
(*and typ_branch global_env ntypes (p, e) = function
  | p, e -> (typ_pattern global_env ntypes p, typ_exp global_env ntypes e)
  | _ ->
      (*Ceci ne sera jamais affiché si le lexing et le parsing sont corrects*)
      failwith
        "You think I hate you? I don't hate you. This job is eating me alive, \
         I can't breathe anymore, and if I come out this guy Lefty dies. \
         They're gonna kill him because he vouched for me because he stood up \
         for me I live with that every day. That's the same thing as if I put \
         the bullet in his head myself, you understand? I spent all these \
         years being the good guy, you know the man in the white fucking hat, \
         for what? For nothing. I'm not becoming like them Maggie, I am them."*)

let rec well_formed_declaration global_env local_env declaration =
  let table = Hashtbl.create (List.length declaration.variables) in
  let rec run = function
    | h :: t ->
        Hashtbl.add table h h;
        run t
    | [] -> ()
  in
  run declaration.variables;
  List.for_all
    (fun i -> well_formed_instance global_env local_env (Intype i))
    declaration.ntypes
  && List.for_all (fun i -> well_formed_typ global_env local_env i) declaration.types

and well_formed_instance global_env local_env = function
  | Intype n -> true
  | Iarrow (nl, n) ->
      if List.for_all (fun x -> well_formed_instance global_env local_env x) nl then
        well_formed_typ n
      else false

and well_formed_typ global_env local_env t =
  match t with
  | Tatype a -> well_formed_atype global_env local_env a
  | Tntype n -> well_formed_ntype global_env local_env n

and well_formed_atype global_env local_env = function
  | Tident id -> (
      try
        find id local_env;
        true
      with
      | Not_found -> failwith "Type non défini"
      | Ttype t -> well_formed_typ global_env local_env t)

and well_formed_ntype global_env local_env (id, al) =
  try
    let l = find id local_env in
    List.length l == List.length al
    && List.for_all (fun x -> well_formed_atype global_env) al
  with Not_found -> failwith "Type non défini"


(* Algorithme : 
  - On parcourt les déclarations du programme dans l'ordre.
  - On initialise l'environnement global de typage avec les déclarations prédéfinies.
  - On ajoute les types des déclarations dans l'environnement global de typage.
  - A chaque fois qu'on doit vérifier un jugement bien formé, on crée un environnement local spécifique. 
*)

let type_decl global_env local_env decl =
  match decl with
  | Defn d -> ()
  | Dfdecl d -> ()
  | Ddata d -> ()
  | Dclass d -> ()
  | Dinstance (i, dl) -> ()