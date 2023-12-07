open Ast
open Pretty

let rec typ_eq t1 t2 =
  match (t1, t2) with
  | TUnit, TUnit | TStr, TStr | TInt, TInt | TBool, TBool -> true
  | TCons (s1, t1), TCons (s2, t2) -> s1 == s2 || List.for_all2 typ_eq t1 t2
  | TArrow (t1, t2), TArrow (t3, t4) ->
      List.for_all2 typ_eq t1 t3 && typ_eq t2 t4
  | TVar t1, TVar t2 -> (
      match (t1.def, t2.def) with
      | None, None -> true
      | Some t1, Some t2 -> typ_eq t1 t2
      | _, _ -> false)
  | TAlias t1, TAlias t2 -> t1 = t2
  | _, _ -> false

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
  | (TInt | TBool | TStr | TUnit | TAlias _) as t -> t
  | TVar { id = a; def = None } as t -> t
  | TVar { id = a; def = Some t } -> TVar { id = a; def = Some (canon t) }
  | TArrow (t1, t2) -> TArrow (List.map canon t1, canon t2)
  | TCons (s, t) -> TCons (s, List.map canon t)

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
  | TCons (s1, t1), TCons (s2, t2) when s1 = s2 -> List.iter2 unify t1 t2
  | TInt, TInt | TBool, TBool | TStr, TStr | TUnit, TUnit -> ()
  | TAlias t1, TAlias t2 when t1 = t2 -> ()
  | t1, t2 -> unification_error t1 t2

module Vset = Set.Make (V)

let rec fvars t =
  match head t with
  | TInt | TStr | TBool | TUnit | TAlias _ -> Vset.empty
  | TArrow (t1, t2) -> List.fold_left Vset.union (fvars t2) (List.map fvars t1)
  | TVar v -> Vset.singleton v
  | TCons (_, t) -> List.fold_left Vset.union Vset.empty (List.map fvars t)

let norm_varset s =
  Vset.fold (fun v s -> Vset.union (fvars (TVar v)) s) s Vset.empty

type schema = { vars : Vset.t; ttyp : ttyp }

module Smaps = Map.Make (String)

type envs = { bindings : schema Smaps.t; fvars : Vset.t }

let empty = { bindings = Smaps.empty; fvars = Vset.empty }

type constr = {
  cident : string;
  ctlist : ttyp list;
  ctyp : ttyp;
  cenvvartyps : (ttyp * int) Smaps.t;
}

let add global x t global_env =
  let vt = fvars t in
  let s, fvars =
    if global then
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
    | (TInt | TBool | TStr | TUnit | TAlias _) as t -> t
    | TArrow (t1, t2) -> TArrow (List.map subst t1, subst t2)
    | TCons (s, t) -> TCons (s, List.map subst t)
  in
  subst tx.ttyp

let rec no_same_name = function
  | [] -> true
  | h :: t -> (not (List.mem h t)) && no_same_name t

let string_to_list s = List.init (String.length s) (String.get s)
let is_lower s = List.mem s.[0] (string_to_list "abcdefghijklmnopqrstuvwxyz")

let rec ast_type = function Tatype a -> ast_atype a | Tntype n -> ast_ntype n

and ast_atype = function
  | Tident "String" -> TStr
  | Tident "TInt" -> TTInt
  | Tident "TBool" -> TBool
  | Tident "Unit" -> TUnit
  | Ttype t -> ast_type t
  | _ -> TBool

and ast_ntype (id, l) =
  let cons_arg = List.map ast_atype l in
  TCons (id, cons_arg)

let frst (a, b, c, d) = a
let scnd (a, b, c, d) = b
let thrd (a, b, c, d) = c
let frth (a, b, c, d) = d

let smaps_find id env =
  try Smaps.find id env with Not_found -> failwith "Identificateur Indéfini"

let function_env =
  ref
    ( List.fold_left2
        (fun env x t -> Smaps.add x t env)
        Smaps.empty
        [ "not"; "mod"; "log"; "pure"; "show" ],
      [
        (TArrow ([ TBool ], TBool), Smaps.empty, None, Smaps.empty);
        (TArrow ([ TInt; TInt ], TInt), Smaps.empty, None, Smaps.empty);
        ( TArrow ([ TStr ], TCons ("Effect", [ TUnit ])),
          Smaps.empty,
          None,
          Smaps.empty );
        ( TArrow ([ TAlias "a" ], TCons ("Effect", [ TAlias "a" ])),
          Smaps.add "a" (TAlias "a", 0) Smaps.empty,
          None,
          Smaps.empty );
        ( TArrow ([ TAlias "a" ], TStr),
          Smaps.add "a" (TAlias "a", 0) Smaps.empty,
          Some "Show",
          Smaps.empty );
      ] )

let cons_env = ref Smaps.empty

let class_env =
  ref
    (Smaps.add "Show"
       ( [ "a" ],
         Smaps.add "show" (TArrow ([ TAlias "a" ], TStr)) Smaps.empty,
         [] )
       Smaps.empty)

let rec typ_exp global_env type_env instance_env loc_expr =
  let loc, expr = loc_expr in
  match expr with
  | Eunop (_, e) -> (
      match typ_exp global_env type_env instance_env type_env e with
      | TInt -> TInt
      | _ ->
          typing_error (fst e)
            "mauvais opérande pour l'opérateur unaire '-' : le type attendu \
             est TInt")
  | Eif (e1, e2, e3) -> (
      match typ_exp global_env type_env instance_env e1 with
      | TBool ->
          let s2 = typ_exp global_env type_env instance_env e2 in
          let s3 = typ_exp global_env type_env instance_env e3 in
          if typ_eq s2 s3 then s2
          else
            typing_error (fst e3) "les deux branches doivent être de même type"
      | _ ->
          typing_error (fst e1)
            "mauvais type de l'expression de condition : le type attendu est \
             Bool")
  | Ebinop (e1, op, e2) -> (
      let s1 =
        typ_exp global_env type_env instance_env type_env instance_env e1
      in
      let s2 =
        typ_exp global_env type_env instance_env type_env instance_env e2
      in
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
               ^ "' : les expressions comparées doivent être de type TInt, \
                  String ou Bool"))
      | Blt | Ble | Bgt | Bge -> (
          match s1 with
          | TInt -> TBool
          | _ ->
              typing_error (fst e1)
                ("mauvais opérande pour l'opérateur '" ^ binop_string
               ^ "' : les expressions comparées doivent être de type TInt"))
      | Bsub | Badd | Bmul | Bdiv -> (
          match s1 with
          | TInt -> TInt
          | _ ->
              typing_error (fst e1)
                ("mauvais opérande pour l'opérateur '" ^ binop_string
               ^ "' : les expressions manipulées doivent être de type TInt"))
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
  | Eatom a -> typ_atom global_env type_env instance_env a
  | Edo l ->
      List.iter
        (fun e ->
          if
            not
              (typ_eq
                 (typ_exp global_env type_env instance_env e)
                 (TCons ("Effect", [ TUnit ])))
          then typing_error (fst e) "le type attendu est Effect Unit")
        l;
      TCons ("Effect", [ TUnit ])
  | Ecase (e, []) -> raise (Empty_pattern_matching (fst e))
  | Ecase (e, l) ->
      let t = typ_exp global_env type_env instance_env e in
      let rettypes =
        List.map (fun x -> typ_branch global_env type_env instance_env x) l
      in
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
           (fun global_env x -> typ_binding global_env type_env instance_env x)
           global_env bl)
        type_env instance_env e
  | Efunc (id, al) -> (
      if not (is_lower id) then (
        let constr = Smaps.find id !cons_env in
        let rec aux al tl =
          match (al, tl) with
          | [], [] -> ()
          | h1 :: t1, h2 :: t2 ->
              if typ_eq (typ_atom global_env type_env instance_env h1) h2 then
                aux t1 t2
              else failwith "Mauvais Types dans le constructeur"
                (*TODO : AJouter localisation*)
          | _ -> failwith "Liste d'argument d'une longueur insuffisante"
        in
        aux al constr.ctlist;
        constr.ctyp)
      else
        try
          let instance_env =
            Smaps.union
              (fun _ a _ -> Some a)
              instance_env
              (frth (Smaps.find id !function_env))
          in
          if Smaps.mem id global_env.bindings then failwith "Pas une fonction"
          else
            match thrd (Smaps.find id !function_env) with
            | None -> TBool
            | Some cident ->
                let instances = Smaps.find cident instance_env in
                compat_instances instance_env cident id instances []
        with Not_found -> failwith "Fonction Non Définie")

and compat_instances instance_env cident id instances tlist = TBool

and typ_atom global_env type_env instance_env (l, a) =
  match a with
  | Aconst c -> (
      match c with Cbool _ -> TBool | Cint _ -> TInt | Cstring _ -> TStr)
  | Aident id -> (
      try find id global_env with Not_found -> raise (Unknown_ident (l, id)))
  | Aexpr e -> typ_exp global_env type_env instance_env e
  | Atypedexpr (e, tau) ->
      let t = ast_type tau in
      if typ_eq t (typ_exp global_env type_env instance_env e) then t
        (* TODO: spécifier les deux types *)
      else typing_error (fst e) "l'expression n'est pas du type spécifié."

and typ_pattern global_env type_env instance_env = function
  | Parg (l, p) -> typ_patarg global_env type_env instance_env (l, p)
  | Pconsarg (id, l) -> TBool (*Pas codé encore*)

and typ_patarg global_env type_env instance_env (l, p) =
  match p with
  | Pconst c -> (
      match c with Cbool _ -> TBool | Cint _ -> TInt | Cstring _ -> TStr)
  | Pident id -> (
      try find id global_env with Not_found -> raise (Unknown_ident (l, id)))
  | Ppattern p -> typ_pattern global_env type_env instance_env p

and typ_binding global_env type_env instance_env = function
  | id, e -> add true id (typ_exp global_env type_env instance_env e) global_env

and typ_branch global_env type_env instance_env (p, e) =
  ( typ_pattern global_env type_env instance_env p,
    typ_exp global_env type_env instance_env e )
(*and typ_branch global_env type_env instance_env    type_env instance_env    (p, e) = function
  | p, e -> (typ_pattern global_env type_env instance_env    type_env instance_env    p, typ_exp global_env type_env instance_env    type_env instance_env    e)
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

let rec well_formed_declaration global_env type_env instance_env local_env
    declaration =
  let table = Hashtbl.create (List.length declaration.variables) in
  let rec run = function
    | h :: t ->
        Hashtbl.add table h h;
        run t
    | [] -> ()
  in
  run declaration.variables;
  List.for_all
    (fun i ->
      well_formed_instance global_env type_env instance_env local_env (Intype i))
    declaration.ntypes
  && List.for_all
       (fun i -> well_formed_typ global_env type_env instance_env local_env i)
       declaration.types

and well_formed_instance global_env type_env instance_env local_env = function
  | Intype (n, n1) -> true
  | Iarrow (nl, n) ->
      if
        List.for_all
          (fun x ->
            well_formed_ntype global_env type_env instance_env local_env x)
          nl
      then well_formed_ntype global_env type_env instance_env local_env n
      else false

and well_formed_typ global_env type_env instance_env local_env t =
  match t with
  | Tatype a -> well_formed_atype global_env type_env instance_env local_env a
  | Tntype n -> well_formed_ntype global_env type_env instance_env local_env n

and well_formed_atype global_env type_env instance_env local_env = function
  | Tident id -> (
      try
        let _ = find id local_env in
        true
      with Not_found -> failwith "Type non défini")
  | Ttype t -> well_formed_typ global_env type_env instance_env local_env t

and well_formed_ntype global_env type_env instance_env local_env (id, al) =
  try
    let l = find id in
    (*On vérifie que le constructeur étudié est bien défini*)
    match l with
    | TCons (s, nl) ->
        (*On représente les constructeurs comme des fonctions dont on vérifie l'arité*)
        List.length nl == List.length al
        && List.for_all
             (fun x ->
               well_formed_atype global_env type_env instance_env local_env x)
             al
    | _ -> failwith "Ceci n'est pas un constructeur"
  with Not_found -> failwith "Constructeur non défini"

(* Algorithme :
   - On parcourt les déclarations du programme dans l'ordre.
   - On initialise l'environnement global de typage avec les déclarations prédéfinies.
   - On ajoute les types des déclarations dans l'environnement global de typage.
   - A chaque fois qu'on doit vérifier un jugement bien formé, on crée un environnement local spécifique.
*)
