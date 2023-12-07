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

let rec unify tl1 tl2 =
  let assoc = ref Smaps.empty in
  let rec aux tl1 tl2 =
    match (tl1, tl2) with
    | [], [] -> true
    | TUnit :: t1, TUnit :: t2
    | TInt :: t1, TInt :: t2
    | TStr :: t1, TStr :: t2
    | TBool :: t1, TBool :: t2 ->
        aux t1 t2
    | TCons (s, _) :: t1, TCons (t, _) :: t2 when t = s -> aux t1 t2
    | TAlias s :: t1, t :: t2 ->
        if Smaps.mem s !assoc then aux (Smaps.find s !assoc :: t1) (t :: t2)
        else (
          assoc := Smaps.add s t !assoc;
          aux t1 t2)
    | t :: t1, TAlias s :: t2 -> aux tl2 tl1
    | _ -> false
  in
  aux tl1 tl2

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

let rec substitute set t =
  match head t with
  | TAlias s -> smaps_find s set
  | TCons (s, tlist) -> TCons (s, List.map (substitute set) tlist)
  | _ as t -> t

let rec typ_exp global_env type_env
    (instance_env : (typ list * (ident * typ list) list) list Smaps.t) global
    loc_expr =
  let loc, expr = loc_expr in
  match expr with
  | Eunop (_, e) -> (
      match typ_exp global_env type_env instance_env global e with
      | TInt -> TInt
      | _ ->
          typing_error (fst e)
            "mauvais opérande pour l'opérateur unaire '-' : le type attendu \
             est TInt")
  | Eif (e1, e2, e3) -> (
      match typ_exp global_env type_env instance_env global e1 with
      | TBool ->
          let s2 = typ_exp global_env type_env instance_env global e2 in
          let s3 = typ_exp global_env type_env instance_env global e3 in
          if typ_eq s2 s3 then s2
          else
            typing_error (fst e3) "les deux branches doivent être de même type"
      | _ ->
          typing_error (fst e1)
            "mauvais type de l'expression de condition : le type attendu est \
             Bool")
  | Ebinop (e1, op, e2) -> (
      let s1 = typ_exp global_env type_env instance_env global e1 in
      let s2 = typ_exp global_env type_env instance_env global e2 in
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
  | Eatom a -> typ_atom global_env type_env instance_env global a
  | Edo l ->
      List.iter
        (fun e ->
          if
            not
              (typ_eq
                 (typ_exp global_env type_env instance_env global e)
                 (TCons ("Effect", [ TUnit ])))
          then typing_error (fst e) "le type attendu est Effect Unit")
        l;
      TCons ("Effect", [ TUnit ])
  | Ecase (e, bl) -> (
      let t = typ_exp global_env type_env instance_env global e in
      match bl with
      | [] -> failwith "Empty Pattern Matching Bro"
      | b :: tail ->
          let tau = typ_branch global_env type_env instance_env t global b in
          List.iter
            (fun x ->
              if
                not
                  (typ_eq tau
                     (typ_branch global_env type_env instance_env t global x))
              then failwith "Mauvais Type de Pattern"
              else ())
            tail;
          if
            not
              (exhaustive_list global_env type_env instance_env [ t ]
                 (List.map (fun x -> [ x ]) (List.map (fun b -> fst b) bl)))
          then failwith "Pattern Non Exhaustif"
          else tau)
  | Elet (bl, e) ->
      let rec aux global_env type_env instance_env l =
        match l with
        | b :: t ->
            let tau = typ_exp global_env type_env instance_env global (snd b) in
            aux (add true (fst b) tau global_env) type_env instance_env t
        | [] -> global_env
      in
      typ_exp
        (aux global_env type_env instance_env bl)
        type_env instance_env global e
  | Efunc (id, al) -> (
      if not (is_lower id) then (
        let constr = Smaps.find id !cons_env in
        let rec aux al tl =
          match (al, tl) with
          | [], [] -> ()
          | h1 :: t1, h2 :: t2 ->
              if typ_eq (typ_atom global_env type_env instance_env global h1) h2
              then aux t1 t2
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
          else (
            (match thrd (Smaps.find id !function_env) with
            | None -> TBool
            | Some cident ->
                let instances = Smaps.find cident instance_env in
                compat_instances instance_env cident id instances
                  (List.map
                     (fun a ->
                       typ_atom global_env type_env instance_env global a)
                     al)
                  global);
            let tlist, t =
              match frst (smaps_find id !function_env) with
              | TArrow (tlist, t) -> (tlist, t)
              | _ -> failwith "Ma qué pasta"
            in

            let vars = ref Smaps.empty in
            let rec aux tlist alist =
              match (tlist, alist) with
              | [], [] -> ()
              | t :: t1, a :: t2 -> (
                  match t with
                  | TAlias s -> (
                      let tau =
                        typ_atom global_env type_env instance_env global a
                      in
                      match tau with
                      | TAlias _ -> aux t1 t2
                      | _ ->
                          if
                            Smaps.mem s !vars
                            && not (typ_eq tau (smaps_find s !vars))
                          then failwith "Types Différents"
                          else vars := Smaps.add s tau !vars;
                          aux t1 t2)
                  | _ ->
                      if
                        not
                          (typ_eq t
                             (typ_atom global_env type_env instance_env global a))
                      then failwith "Mauvais Type d'Argument"
                      else aux t1 t2)
              | _ -> failwith "Trop d'arguments"
            in
            aux tlist al;
            substitute !vars t)
        with Not_found -> failwith "Fonction Non Définie")

and compat_instances instance_env cident id instances tlist global =
  let slist, class_functions, functions = smaps_find cident !class_env in
  let ftlist =
    match smaps_find id class_functions with
    | TArrow (tl, t) -> tl
    | _ -> failwith "Ma qué pasta ?"
  in
  let rec consmap env tl1 tl2 =
    match (tl1, tl2) with
    | [], [] -> env
    | TAlias s :: t1, h :: t2 -> Smaps.add s h (consmap env t1 t2)
    | t1 :: q1, t2 :: q2 when typ_eq t1 t2 -> consmap env q1 q2
    | _ -> failwith "Appel incompatible avec sa définition"
  in
  let env = consmap Smaps.empty ftlist tlist in
  let variables = List.map (fun s -> smaps_find s env) slist in
  let rec valid tl1 tl2 =
    match (tl1, tl2) with
    | h1 :: t1, h2 :: t2 when h1 = h2 -> valid t1 t2
    | [], [] -> true
    | _ -> false
  in
  let rec basic_instinct instance_env tlist = function
    | [] -> false
    | h :: t ->
        (((not global) && valid h tlist) || (global && unify h tlist))
        || basic_instinct instance_env tlist t
  in
  let rec all_basic_instinct instance_env = function
    | [] -> true
    | (s, tlist) :: q ->
        basic_instinct instance_env tlist
          (List.map fst (smaps_find s instance_env))
        && all_basic_instinct instance_env q
  in
  let rec find_valid tlist = function
    | [] ->
        failwith
          ("Pas d'instance compatible pour la classe " ^ cident
         ^ " en appelant " ^ f)
    | tl :: q
      when (((not global) && valid (fst tl) tlist)
           || (global && unify (fst tl) tlist))
           && all_basic_instinct instance_env (snd tl) ->
        ()
    | tl :: q -> find_valid tlist q
  in
  find_valid tlist instances

and typ_atom global_env type_env instance_env global (l, a) =
  match a with
  | Aconst c -> (
      match c with Cbool _ -> TBool | Cint _ -> TInt | Cstring _ -> TStr)
  | Aident id when is_lower id -> (
      try find id global_env with Not_found -> raise (Unknown_ident (l, id)))
  | Aident id -> (smaps_find id !cons_env).ctyp
  | Aexpr e -> typ_exp global_env type_env instance_env global e
  | Atypedexpr (e, tau) ->
      let t = ast_type global_env type_env instance_env tau in
      if typ_eq t (typ_exp global_env type_env instance_env global e) then t
        (* TODO: spécifier les deux types *)
      else typing_error (fst e) "l'expression n'est pas du type spécifié."

and typ_branch global_env type_env instance_env t global branch =
  let env_pat = typ_pattern empty type_env instance_env t (fst branch) in
  let global_env =
    {
      bindings =
        Smaps.union (fun _ a _ -> Some a) env_pat.bindings global_env.bindings;
      fvars = Vset.union env_pat.fvars global_env.fvars;
    }
  in
  typ_exp global_env type_env instance_env global (snd branch)

and typ_pattern global_env type_env instance_env t = function
  | Parg p -> typ_patarg global_env type_env instance_env t p
  | Pconsarg (id, plist) ->
      let cons = smaps_find id !cons_env in
      if not (typ_eq cons.ctyp t) then failwith "Mauvais type de constructeur"
      else
        let rec aux env plist tlist =
          match (plist, tlist) with
          | [], [] -> env
          | p :: t1, t :: t2 ->
              let a = typ_patarg env type_env instance_env t p in
              let env = aux env t1 t2 in
              {
                bindings =
                  Smaps.union
                    (fun s _ _ -> failwith "Identifiant utilisé plusieurs fois")
                    env.bindings a.bindings;
                fvars = Vset.union a.fvars env.fvars;
              }
          | _ -> failwith "Mauvaise Arité pour le Constructeur"
        in
        aux global_env plist cons.ctlist

and typ_patarg global_env type_env instance_env t = function
  | l, Pconst c ->
      let ctau =
        match c with Cbool _ -> TBool | Cint _ -> TInt | Cstring _ -> TStr
      in
      if not (typ_eq ctau t) then failwith "Mauvais Type" else global_env
  | l, Pident s when is_lower s -> add false s t global_env
  | l, Pident s ->
      if not (typ_eq (smaps_find s !cons_env).ctyp t) then
        failwith "Mauvais Type de Constructeur"
      else global_env
  | l, Ppattern p -> typ_pattern global_env type_env instance_env t p

and ast_type global_env type_env instance_env = function
  | Tatype a -> ast_atype global_env type_env instance_env a
  | Tntype n -> ast_ntype global_env type_env instance_env n

and ast_atype global_env type_env instance_env = function
  | Tident s ->
      let t, arity = smaps_find s type_env in
      if arity = 0 then t else failwith "Mauvaise arité de constructeur/de type"
  | Ttype t -> ast_type global_env type_env instance_env t

and ast_ntype global_env type_env instance_env (id, al) =
  let t, arity = smaps_find id type_env in
  if List.length al <> arity then failwith "Mauvaise Arité de Constructeur"
  else
    match t with
    | TCons (s, _) ->
        TCons (s, List.map (ast_atype global_env type_env instance_env) al)
    | _ -> t

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

(* Le bloc ci-dessous est commenté suite à un changement de perspective. Le bloc ci-dessus est commenté parce que je code avec les pieds. *)
(* let rec well_formed_declaration global_env type_env instance_env local_env
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
       let l = find id global_env in
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
     with Not_found -> failwith "Constructeur non défini" *)

(* Algorithme :
   - On parcourt les déclarations du programme dans l'ordre.
   - On initialise l'environnement global de typage avec les déclarations prédéfinies.
   - On ajoute les types des déclarations dans l'environnement global de typage.
   - A chaque fois qu'on doit vérifier un jugement bien formé, on crée un environnement local spécifique.
*)
