open Ast
open Pretty

let rec typ_eq t1 t2 =
  match (t1, t2) with
  | TUnit, TUnit | TStr, TStr | TInt, TInt | TBool, TBool -> true
  (*| TEffect t1, TEffect t2 -> typ_eq t1 t2*)
  | TCons(s1, t1), TCons(s2, t2) -> s1 == s2 || List.for_all2 typ_eq t1 t2
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
  (*| TEffect t1 -> TEffect (canon t1)*)
  | TVar { id = a; def = None } as t -> t
  | TVar { id = a; def = Some t } -> TVar { id = a; def = Some (canon t) }
  | TArrow (t1, t2) -> TArrow (List.map canon t1, canon t2)
  | TCons(s, t) -> TCons (s,List.map canon t)

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
  | TCons(s1, t1), TCons(s2, t2) -> List.iter2 unify t1 t2
  | TInt, TInt | TBool, TBool | TStr, TStr | TUnit, TUnit -> ()
  (* | TEffect t1, TEffect t2 when typ_eq t1 t2 -> () *)
  | t1, t2 -> unification_error t1 t2

module Vset = Set.Make (V)

let rec fvars t =
  match head t with
  | TInt | TStr | TBool | TUnit -> Vset.empty
  (* | TEffect t -> fvars t *)
  | TArrow (t1, t2) -> List.fold_left Vset.union (fvars t2) (List.map fvars t1)
  | TVar v -> Vset.singleton v
  | TCons(s, t) -> List.fold_left Vset.union Vset.empty (List.map fvars t)

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
    (* | TEffect t -> TEffect (subst t) *)
    | TArrow (t1, t2) -> TArrow (List.map subst t1, subst t2)
    | TCons(s,  t) -> TCons (s, List.map subst t)
  in
  subst tx.ttyp

let rec ast_type = function Tatype a -> ast_atype a | Tntype n -> ast_ntype n

and ast_atype = function
  | Tident "String" -> TStr
  | Tident "Int" -> TInt
  | Tident "Boolean" -> TBool
  | Tident "Unit" -> TUnit
  | Ttype t -> ast_type t
  | _ -> TBool

and ast_ntype (id, l) =
  let cons_arg = List.map ast_atype l in
  TCons (id, cons_arg)

let rec check_exhaustivity global_env local_env tau patarglist =
  let rec has_id = function
    | [] -> false
    | Parg (l, Pident _) :: _ -> true
    | Parg (l, Ppattern p) :: q -> has_id [ p ] || has_id q
    | _ :: q -> has_id q
  in
  let rec has_bool b = function
    | [] -> false
    | Parg (l, Pconst (Cbool x)) :: _ when x = b -> true
    | Parg (l, Ppattern p) :: q -> has_bool b [ p ] || has_bool b q
    | _ :: q -> has_bool b q
  in
  let rec has_constr cs = function 
    | [] -> false
    | Pconsarg (s, _)::_ when s = cs -> true
    | Parg(l, Pident s)::_ when s = cs -> true
    | Parg(l, Ppattern p)::q -> has_constr cs [p] || has_constr cs q
    | _::q -> has_constr cs q in
  if has_id patarglist then true
  else match tau with
  | TBool -> has_bool false patarglist && has_bool true patarglist
  | TCons(s, _) -> has_constr s patarglist 
  | _ -> false

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
  | Eatom a -> typ_atom global_env local_env a
  | Edo l -> failwith "TODO"
      (* List.iter
        (fun e ->
          if not (typ_eq (typ_exp global_env local_env e) (TEffect TUnit)) then
            typing_error (fst e) "le type attendu est Unit")
        l;
      TEffect TUnit *)
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
        (List.fold_left
           (fun global_env x -> typ_binding global_env local_env x)
           global_env bl)
        local_env e
  | Efunc (id, al) ->
      let t1 = find id global_env in
      let t2 = List.map (fun x -> typ_atom global_env local_env x) al in
      let v = TVar (V.create ()) in
      unify t1 (TArrow (t2, v));
      v

and typ_atom global_env local_env (l, a) =
  match a with
  | Aconst c -> (
      match c with Cbool _ -> TBool | Cint _ -> TInt | Cstring _ -> TStr)
  | Aident id -> (
      try find id global_env with Not_found -> raise (Unknown_ident (l, id)))
  | Aexpr e -> typ_exp global_env local_env e
  | Atypedexpr (e, tau) ->
      let t = ast_type tau in
      if typ_eq t (typ_exp global_env local_env e) then t
        (* TODO: spécifier les deux types *)
      else typing_error (fst e) "l'expression n'est pas du type spécifié."

and typ_pattern global_env local_env = function
  | Parg (l, p) -> typ_patarg global_env local_env (l, p)
  | Pconsarg (id, l) -> TBool (*Pas compris encore*)

and typ_patarg global_env local_env (l, p) =
  match p with
  | Pconst c -> (
      match c with Cbool _ -> TBool | Cint _ -> TInt | Cstring _ -> TStr)
  | Pident id -> (
      try find id global_env with Not_found -> raise (Unknown_ident (l, id)))
  | Ppattern p -> typ_pattern global_env local_env p

and typ_binding global_env local_env = function
  | id, e -> add true id (typ_exp global_env local_env e) global_env

and typ_branch global_env local_env (p, e) =
  (typ_pattern global_env local_env p, typ_exp global_env local_env e)
(*and typ_branch global_env local_env (p, e) = function
  | p, e -> (typ_pattern global_env local_env p, typ_exp global_env local_env e)
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
  && List.for_all
       (fun i -> well_formed_typ global_env local_env i)
       declaration.types

and well_formed_instance global_env local_env = function
  | Intype (n, n1) -> true
  | Iarrow (nl, n) ->
      if List.for_all (fun x -> well_formed_ntype global_env local_env x) nl
      then well_formed_ntype global_env local_env n
      else false

and well_formed_typ global_env local_env t =
  match t with
  | Tatype a -> well_formed_atype global_env local_env a
  | Tntype n -> well_formed_ntype global_env local_env n

and well_formed_atype global_env local_env = function
  | Tident id -> (
      try
        let _ = find id local_env in
        true
      with Not_found -> failwith "Type non défini")
  | Ttype t -> well_formed_typ global_env local_env t

and well_formed_ntype global_env local_env (id, al) =
  try
    let l = find id global_env in
    (*On vérifie que le constructeur étudié est bien défini*)
    match l with
    | TCons(s, nl) ->
        (*On représente les constructeurs comme des fonctions dont on vérifie l'arité*)
        List.length nl == List.length al
        && List.for_all (fun x -> well_formed_atype global_env local_env x) al
    | _ -> failwith "Ceci n'est pas un constructeur"
  with Not_found -> failwith "Type non défini"

(* Algorithme :
   - On parcourt les déclarations du programme dans l'ordre.
   - On initialise l'environnement global de typage avec les déclarations prédéfinies.
   - On ajoute les types des déclarations dans l'environnement global de typage.
   - A chaque fois qu'on doit vérifier un jugement bien formé, on crée un environnement local spécifique.
*)
let rec type_defn global_env local_env (id, plist, exp) =
  add false id (typ_exp global_env empty exp) global_env

and type_fdecl global_env local_env
    { name = id; variables = idl; ntypes = nl; types = tl; out_type = t } =
  if
    well_formed_declaration global_env local_env
      { name = id; variables = idl; ntypes = nl; types = tl; out_type = t }
  then add true id (TArrow (List.map ast_ntype nl, ast_type t)) global_env
  else failwith "La déclaration est mal formée."
(*GRRRRRRRRRRRRRRR OCAML BORDEL FAIS TON TYPAGE AUTREMENT QU'AVEC LE CUL CONNARD*)

and type_class global_env local_env { name = id; params = idl; decls = defl } =
  let local_env =
    List.fold_left (fun env x -> add false x TUnit env) local_env idl
  in
  if Smaps.cardinal local_env.bindings <> List.length idl then
    failwith "Il faut changer les noms des variables";
  List.fold_left (fun env def -> type_fdecl env local_env def) global_env defl

and type_instance global_env local_env (i, dl) =
  let rec run = function [] -> () | h :: t -> () in
  run dl;
  global_env

and type_data global_env local_env { name = id; params = idl; types = ntl } =
  global_env

let type_decl global_env local_env decl =
  match decl with
  | Defn d -> type_defn global_env local_env d
  | Dfdecl d -> type_fdecl global_env local_env d
  | Ddata d -> global_env (*This is pas parsé correctly*)
  | Dclass d -> type_class global_env empty d
  | Dinstance (i, dl) -> type_instance global_env empty (i, dl)

let fonctionstypesettrucstraditionnelsalanoix = []

let base_env =
  let e = empty in
  List.fold_left
    (fun env x -> add false (fst x) (snd x) e)
    e fonctionstypesettrucstraditionnelsalanoix

let type_file file =
  let env = base_env in
  List.fold_left (fun env x -> type_decl env empty x) env file
