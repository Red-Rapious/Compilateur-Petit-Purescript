open Ast
open Pretty

exception TypingError of loc * string
exception EmptyPatternMatching of loc
exception UnknownIdent of loc * Ast.ident
exception NonExhaustivePatternMatching of loc
exception TooManyArguments of loc
exception IdentUsedTwice of loc * string (* localisation * identifiant *)
exception MultipleFiltering of loc
exception SimilarNames of loc * string (* localisation * nom de l'objet *)
exception EmptyDefinition of loc
exception DoubleDefinition of loc * string (* localisation * identifiant *)
exception AlreadyDefined of loc * string * string (* localisation * nom de l'objet * nom spécifique déjà défini *)
exception UnifiableInstances of loc
exception MissingDefinition of loc
exception MissingMain
exception MultipleFilteredVariables of loc
exception BadTypesNumber of loc

let rec head = function TVar { def = Some t } -> head t | t -> t

let rec canon t =
  match head t with
  | (TInt | TBool | TStr | TUnit | TAlias _) as t -> t
  | TVar { id = a; def = None } as t -> t
  | TVar { id = a; def = Some t } -> TVar { id = a; def = Some (canon t) }
  | TArrow (t1, t2) -> TArrow (List.map canon t1, canon t2)
  | TCons (s, t) -> TCons (s, List.map canon t)


let typing_error loc s = raise (TypingError (loc, s))

let placeholder_loc = Lexing.dummy_pos, Lexing.dummy_pos

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

type type_env = (ttyp * int) Smaps.t
type instance_env = (ttyp list * (ident * ttyp list) list) list Smaps.t

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

let is_lower s = s = String.lowercase_ascii s
let frst (a, b, c, d) = a
let scnd (a, b, c, d) = b
let thrd (a, b, c, d) = c
let frth (a, b, c, d) = d

let smaps_find id env =
  try Smaps.find id env
  with Not_found -> raise (UnknownIdent (placeholder_loc, id))

(*Cette partie du code a été écrite par Matthieu Boyer, en s'étant inspiré de l'architecture d'environnement du code de Nathan Boyer *)
let function_env =
  ref
    (List.fold_left2
       (fun env x t -> Smaps.add x t env)
       Smaps.empty
       [ "not"; "mod"; "log"; "pure"; "show" ]
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
       ])

let cons_env = ref Smaps.empty

let (global_env_instances :
      (ttyp list * (ident * ttyp list) list) list Smaps.t ref) :
    (ttyp list * (ident * ttyp list) list) list Smaps.t ref =
  ref (Smaps.add "Show" [ ([ TInt ], []); ([ TBool ], []) ] Smaps.empty)

let class_env =
  ref
    (Smaps.add "Show"
       ( [ "a" ],
         Smaps.add "show" (TArrow ([ TAlias "a" ], TStr)) Smaps.empty,
         [] )
       Smaps.empty)

let curr_defined = ref ""
let (eqlist : defn list ref) = ref []

let rec substitute set t =
  match head t with
  | TAlias s -> smaps_find s set
  | TCons (s, tlist) -> TCons (s, List.map (substitute set) tlist)
  | _ as t -> t

let rec typ_exp global_env type_env
    (instance_env : (ttyp list * (ident * ttyp list) list) list Smaps.t) global
    loc_expr =
  let loc, expr = loc_expr in
  match expr with
  | Eunop (_, e) -> (
      match typ_exp global_env type_env instance_env global e with
      | TInt -> TInt
      | _ ->
          typing_error (fst e)
            "mauvais opérande pour l'opérateur unaire '-' : le type attendu est TInt")
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
          | TInt | TBool | TStr | TUnit -> TBool
          | _ ->
              typing_error (fst e1)
                ("mauvais opérande pour l'opérateur '" ^ binop_string
               ^ "' : les expressions comparées doivent être de type TInt, \
                  String ou Bool"))
      | Blt | Ble | Bgt | Bge -> (
        (* TODO: convertir les Bgt et Bge en Blt et Ble en inversant les deux expressions *)
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
      | [] -> raise (EmptyPatternMatching (fst e))
      | b :: tail ->
          let tau = typ_branch global_env type_env instance_env t global b in
          List.iter
            (fun x ->
              if
                not
                  (typ_eq tau
                     (typ_branch global_env type_env instance_env t global x))
              then raise (TypingError (fst e, "mauvais type")))
            tail;
          if
            not
              (exhaustive_list global_env type_env instance_env [ t ]
                 (List.map (fun x -> [ x ]) (List.map (fun b -> fst b) bl)))
          then raise (NonExhaustivePatternMatching (fst e))
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
              else typing_error loc "mauvais types dans le constructeur"
          | _ -> typing_error loc "liste d'arguments de longueur insuffisante"
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
          if Smaps.mem id global_env.bindings then
            typing_error loc "ceci n'est pas une fonction"
          else (
            (match thrd (Smaps.find id !function_env) with
            | None -> ()
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
              | _ -> failwith "dans typ_expr, le type récupéré n'est pas TArrow"
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
                          then typing_error (fst a) "les types sont différents"
                          else vars := Smaps.add s tau !vars;
                          aux t1 t2)
                  | _ ->
                      if
                        not
                          (typ_eq t
                             (typ_atom global_env type_env instance_env global a))
                      then typing_error loc "mauvais type d'argument"
                      else aux t1 t2)
              | _ -> raise (TooManyArguments loc)
            in
            aux tlist al;
            substitute !vars t)
        with Not_found -> raise (UnknownIdent (loc, id)))

and compat_instances instance_env cident id instances tlist global =
  let slist, class_functions, functions = smaps_find cident !class_env in
  let ftlist =
    match smaps_find id class_functions with
    | TArrow (tl, t) -> tl
    | _ -> failwith "dans ftlist, le type récupéré n'est pas TArrow"
  in
  let rec consmap env tl1 tl2 =
    match (tl1, tl2) with
    | [], [] -> env
    | TAlias s :: t1, h :: t2 -> Smaps.add s h (consmap env t1 t2)
    | t1 :: q1, t2 :: q2 when typ_eq t1 t2 -> consmap env q1 q2
    | _ ->
        typing_error
          placeholder_loc
          "appel incompatible avec la définition"
  in
  let env = consmap Smaps.empty ftlist tlist in
  let variables = List.map (fun s -> smaps_find s env) slist in
  let rec valid tl1 tl2 =
    match (tl1, tl2) with
    | h1 :: t1, h2 :: t2 when h1 = h2 -> valid t1 t2
    | [], [] -> true
    | _ -> false
  in
  let rec compatible tlist = function
    | [] -> false
    | h :: t ->
        (((not global) && valid h tlist) || (global && unify h tlist))
        || compatible tlist t
  in
  let rec all_compatible
      (instance_env : (ttyp list * (ident * ttyp list) list) list Smaps.t) =
    function
    | [] -> true    
    | (s, tlist) :: q ->
      compatible tlist (List.map fst (smaps_find s instance_env))
        && all_compatible instance_env q
  in
  let rec find_valid tlist = function
    | [] ->
        typing_error
          placeholder_loc
          "instances incompatibles"
    | tl :: q -> if not (
      (
        ((not global) && valid (fst tl) tlist)
        || (global && unify (fst tl) tlist)
      )
      && all_compatible instance_env (snd tl)
    ) then find_valid tlist q
  in
  find_valid variables instances

and typ_atom global_env type_env instance_env global (l, a) =
  match a with
  | Aconst c -> (
      match c with Cbool _ -> TBool | Cint _ -> TInt | Cstring _ -> TStr)
  | Aident id when is_lower id -> (
      try find id global_env with Not_found -> raise (UnknownIdent (l, id)))
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
      if not (typ_eq cons.ctyp t) then
        typing_error (fst (fst (List.hd plist)), snd (fst (List.nth plist ((List.length plist) - 1)))) ("le constructeur '" ^ id ^"' est de mauvais type")
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
                    (fun s _ _ -> raise (IdentUsedTwice (fst p, s)))
                    env.bindings a.bindings;
                fvars = Vset.union a.fvars env.fvars;
              }
          | _ ->
              typing_error (fst (List.hd plist))
                "mauvaise arité pour le constructeur"
        in
        aux global_env plist cons.ctlist

and typ_patarg global_env type_env instance_env t = function
  | l, Pconst c ->
      let ctau =
        match c with Cbool _ -> TBool | Cint _ -> TInt | Cstring _ -> TStr
      in
      if not (typ_eq ctau t) then
        typing_error l "mauvais type d'argument de pattern"
      else global_env
  | l, Pident s when is_lower s -> add false s t global_env
  | l, Pident s ->
      if not (typ_eq (smaps_find s !cons_env).ctyp t) then
        typing_error l "mauvais type de constructeur"
      else global_env
  | l, Ppattern p -> typ_pattern global_env type_env instance_env t p

and ast_type global_env type_env instance_env = function
  | Tatype a -> ast_atype global_env type_env instance_env a
  | Tntype n -> ast_ntype global_env type_env instance_env n

and ast_atype global_env type_env instance_env = function
  | Tident s ->
      let t, arity = smaps_find s type_env in
      if arity = 0 then t else typing_error placeholder_loc ("mauvaise arité du constructeur/type " ^ s)
  | Ttype t -> ast_type global_env type_env instance_env t

and ast_ntype global_env type_env instance_env (id, al) =
  let t, arity = smaps_find id type_env in
  if List.length al <> arity then typing_error placeholder_loc ("mauvaise arité du constructeur " ^ id)
  else
    match t with
    | TCons (s, _) ->
        TCons (s, List.map (ast_atype global_env type_env instance_env) al)
    | _ -> t

and exhaustive_list global_env (type_env : type_env) (instance_env : instance_env) (tlist : ttyp list) (pllist : pattern list list) =
  let rec is_ident = function
    | Parg (l, Pident s) when is_lower s -> true
    | Parg (l, Ppattern p) -> is_ident p
    | _ -> false
  in
  let rec is_bool b = function
    | Parg (l, Pconst (Cbool x)) -> x = b
    | Parg (l, Ppattern p) -> is_bool b p
    | _ -> false
  in
  let rec is_cons c = function
    | Parg (l, Pident s) when s = c -> true
    | Pconsarg (s, _) when s = c -> true
    | Parg (l, Ppattern p) -> is_cons c p
    | _ -> false
  in
  let rec get_list = function
    | [] -> []
    | (Pconsarg (_, x) :: _) :: t -> List.map (fun t -> Parg t) x :: get_list t
    | (Parg (l, Pident s) :: _) :: t when not (is_lower s) -> get_list t
    | (Parg (l, Ppattern p) :: t1) :: t2 -> get_list ((p :: t1) :: t2)
    | _ -> failwith "une erreur de syntaxe a été détéctée lors du typage"
  in
  let rec is_id_in_list f i = function
    | [] -> []
    | x :: t when f (List.nth x i) -> x :: is_id_in_list f i t
    | _ :: t -> is_id_in_list f i t
  in

  let rec aux i l =
    if l = [] then false
    else if i >= List.length tlist then true
    else
      match List.nth tlist i with
      | TBool ->
          aux (i + 1) (is_id_in_list is_ident i l)
          || aux (i + 1) (is_id_in_list (is_bool true) i l)
             && aux (i + 1) (is_id_in_list (is_bool false) i l)
      | TCons (s, []) ->
          aux (i + 1) (is_id_in_list is_ident i l)
          || Smaps.for_all
               (fun _ c ->
                 (not (typ_eq c.ctyp (List.nth tlist i)))
                 || aux (i + 1) (is_id_in_list (is_cons c.cident) i l)
                    && exhaustive_list global_env type_env instance_env c.ctlist
                         (get_list (is_id_in_list (is_cons c.cident) i l)))
               !cons_env
      | _ -> aux (i + 1) (is_id_in_list is_ident i l)
  in
  List.length tlist = 0 || aux 0 pllist

(* Fonctions utilitaires pour accéder aux éléments d'un 3-uplet *)
let fast (a, b, c) = a
let sand (a, b, c) = b
let trad (a, b, c) = c

(* TODO: vérifier pourquoi global_env n'est pas inféré en type ('a) *)
let verify_def global_env (type_env : type_env) (instance_env : instance_env) =
  let rec is_ident = function
    | Pident s -> true
    | Ppattern (Parg (l, p)) -> is_ident p
    | _ -> false
  in
  let rec no_dups found i = function
    | [] -> -1
    | h :: t when is_ident h -> no_dups found (i + 1) t
    | h :: t when found -> raise (MultipleFiltering placeholder_loc)
    | h :: t ->
        let _ = no_dups true (i + 1) t in
        i
  in
  let rec aux col definition = List.nth (sand definition) col in
  if !curr_defined = "" then ()
  else
    let type_env =
      Smaps.union
        (fun _ _ _ -> raise (SimilarNames (placeholder_loc, "variables")))
        type_env
        (scnd (smaps_find !curr_defined !function_env))
    in
    eqlist := List.rev !eqlist;
    match !eqlist with
    | [] -> raise (EmptyDefinition placeholder_loc)
    | h :: t -> (
        let col = no_dups false 0 (List.map snd (sand h)) in
        if col = -1 then (
          if List.length !eqlist > 1 then (raise (DoubleDefinition (placeholder_loc, fast h)))
          else curr_defined := "";
          eqlist := []
        )
        else
          match frst (smaps_find !curr_defined !function_env) with
          | TArrow (tlist, t) ->
              if
                not
                  (exhaustive_list empty type_env instance_env
                     [ List.nth tlist col ]
                     (List.map
                        (fun x -> [ Parg(placeholder_loc ,x) ])
                        (List.map
                           (fun defn ->
                             aux col
                               (fast defn, List.map snd (sand defn), trad defn))
                           !eqlist)))
              then
                raise (NonExhaustivePatternMatching placeholder_loc)
              else curr_defined := "";
              eqlist := []
          | _ -> failwith "dans verify_def, le type récupéré n'est pas TArrow")

(* Typage d'une déclaration de fonction *)
let rec typ_fdecl (global_env : envs) (type_env : type_env) (instance_env : instance_env) (fdecl : fdecl) =
  (* on vérifie que la fonction n'est pas déjà définie ailleurs *)
  if Smaps.mem fdecl.name !function_env then (raise (IdentUsedTwice (placeholder_loc, fdecl.name)))
  else if not (no_same_name fdecl.variables) then
    raise (SimilarNames (placeholder_loc, "variables"))
  else
    let var_type_env =
      List.fold_left
        (fun t_env s -> Smaps.add s (TAlias s, 0) t_env)
        Smaps.empty fdecl.variables
    in
    let type_env =
      Smaps.union
        (fun _ _ _ -> raise (SimilarNames (placeholder_loc, "patterns")))
        type_env var_type_env
    in
    let instance_env =
      List.fold_left
        (fun inst_env n ->
          Smaps.add (fst n)
            ((List.map (ast_atype global_env type_env inst_env) (snd n), [])
            :: smaps_find (fst n) inst_env)
            inst_env)
        instance_env fdecl.ntypes
    in
    curr_defined := fdecl.name;
    ( TArrow
        ( List.map (ast_type global_env type_env instance_env) fdecl.types,
          ast_type global_env type_env instance_env fdecl.out_type ),
      var_type_env,
      instance_env )

(* Typage d'une définition *)
and typ_defn global_env type_env instance_env deflist (defn : defn) tlist t =
  if deflist && Smaps.mem (fast defn) !function_env && !curr_defined <> fast defn
  then raise (DoubleDefinition (placeholder_loc, (fast defn)))
  else (
    if deflist then eqlist := defn :: !eqlist;
    let (plist : loc_patarg list) = sand defn in
    let global_env =
      if List.length plist <> List.length tlist then raise (BadTypesNumber (fst (List.nth plist ((List.length plist) - 1)))) ;
      List.fold_right2
        (fun pt t envi ->
          let a = typ_patarg empty type_env instance_env t pt in
          {
            bindings =
              Smaps.union
                (fun _ _ _ -> raise (SimilarNames (placeholder_loc, "bindings")))
                a.bindings envi.bindings;
            fvars = Vset.union a.fvars envi.fvars;
          })
        plist tlist global_env
    in
    let type_env =
      if deflist then
        Smaps.union
          (fun _ _ _ -> raise (SimilarNames (placeholder_loc, "variables")))
          type_env
          (scnd (smaps_find (fast defn) !function_env))
      else type_env
    in
    if
      not
        (typ_eq
           (typ_exp global_env type_env instance_env deflist (trad defn))
           t)
    then typing_error placeholder_loc "mauvais type de retour")


(* Typage d'une déclaration "data" *)
and typ_data (global_env : envs) (type_env : type_env ref) (instance_env : instance_env) (data : data) =
  if Smaps.mem data.name !type_env then raise (AlreadyDefined (placeholder_loc, "Un type", data.name))
  else if not (no_same_name data.params) then
    raise (SimilarNames (placeholder_loc, "variables"))
  else
    let tau = TCons (data.name, []) in
    type_env := Smaps.add data.name (tau, List.length data.params) !type_env;
    let var_type_env =
      List.fold_left
        (fun env id -> Smaps.add id (TAlias id, 0) env)
        Smaps.empty data.params
    in
    let real_type_env =
      Smaps.union
        (fun _ _ _ -> raise (SimilarNames (placeholder_loc, "variables")))
        var_type_env !type_env
    in
    let rec add_cons = function
      | [] -> ()
      | (i, al) :: t ->
          if Smaps.mem i !cons_env then raise (AlreadyDefined (placeholder_loc, "Un constructeur", data.name))
          else
            cons_env :=
              Smaps.add i
                {
                  cident = i;
                  ctlist =
                    List.map
                      (ast_atype global_env real_type_env instance_env)
                      al;
                  ctyp = tau;
                  cenvvartyps = var_type_env;
                }
                !cons_env;
          add_cons t
    in
    add_cons data.types; 

(* Typage d'une classe *)
and typ_class (global_env : envs) (type_env : type_env) (instance_env : instance_env) (clas : clas) =
  if Smaps.mem clas.name !class_env then raise (AlreadyDefined (placeholder_loc, "Une classe", clas.name))
  else
    let class_functions = ref Smaps.empty in
    List.iter
      (fun fdecl ->
        let tau, var_env, inst_env =
          typ_fdecl global_env type_env instance_env fdecl
        in
        curr_defined := "";
        eqlist := [];
        class_functions := Smaps.add fdecl.name tau !class_functions;
        function_env :=
          Smaps.add fdecl.name
            (tau, var_env, Some clas.name, inst_env)
            !function_env)
      (List.map
         (fun { name; variables = vars; ntypes = nl; types = tl; out_type = t } ->
           {
             name;
             variables = clas.params;
             ntypes = nl;
             types = tl;
             out_type = t;
           })
         clas.decls);
         class_env := Smaps.add clas.name (clas.params, !class_functions, []) !class_env;
         global_env_instances := Smaps.add clas.name [] !global_env_instances

and add_atype (type_env : type_env) = function
  | Tident s :: q when is_lower s ->
      Smaps.add s (TAlias s, 0) (add_atype type_env q)
  | Tident _ :: q -> add_atype type_env q
  | Ttype t :: q ->
      Smaps.union
        (fun _ a _ -> Some a)
        (add_atype type_env q) (add_ast_type type_env t)
  | [] -> type_env

and add_ast_type (type_env : type_env) = function
  | Tatype a -> add_atype type_env [ a ]
  | Tntype n -> add_ntype type_env n

and add_ntype type_env n = add_atype type_env (snd n)

and add_ntype_list type_env = function
  | [] -> type_env
  | n :: q ->
      Smaps.union
        (fun _ a _ -> Some a)
        (add_ntype type_env n)
        (add_ntype_list Smaps.empty q)

(* Typage d'une instance *)
and typ_instance global_env (type_env : type_env) (instance_env : instance_env) (dinst : instance * defn list) =
  match fst dinst with
  | Intype n ->
      typ_instance global_env type_env instance_env (Iarrow ([], n), snd dinst)
  | Iarrow (nl, n) ->
  if Smaps.mem (fst n) instance_env then raise (DoubleDefinition (placeholder_loc, (fst n)))
  else
      let cons_id = fst n in
      let sl, class_functions, fl = smaps_find cons_id !class_env in
      let rec add_instance instance_env type_env = function
        | [] -> instance_env
        | n :: q ->
            Smaps.add (fst n)
              (( List.map (ast_atype global_env type_env instance_env) (snd n),
                 [] )
              :: smaps_find (fst n) instance_env)
              (add_instance instance_env type_env q)
      in
      let type_env = add_ntype_list type_env nl in
      let type_env = add_atype type_env (snd n) in
      let instance_env = add_instance instance_env type_env nl in
      let aux tl1 tl2 = if unify tl1 tl2 then raise (UnifiableInstances placeholder_loc) in
      List.iter
        (aux (List.map (ast_atype global_env type_env instance_env) (snd n)))
        (List.map fst (smaps_find (fst n) !global_env_instances));

      (* à partir d'une liste d'ident et d'une liste de 'a, renvoie une Smap contenant les bindings (ident, 'a) *)
      let rec aux sl tl =
        match (sl, tl) with
        | [], [] -> Smaps.empty
        | s :: t1, t :: t2 -> Smaps.add s t (aux t1 t2)
        | _ -> raise (BadTypesNumber placeholder_loc)
      in

      let sub_table =
        aux sl (List.map (ast_atype global_env type_env instance_env) (snd n))
      in
      let sub = function TAlias s -> smaps_find s sub_table | t -> t in
      List.iter
        (fun (defn : defn) ->
          match frst (smaps_find (fast defn) !function_env) with
          | TArrow (tlist, t) ->
              typ_defn global_env type_env instance_env false defn
                (List.map sub tlist) (sub t)
          | _ -> failwith "dans typ_instance, le type récupéré n'est pas TArrow")
        fl;
      let rec is_ident = function
        | Pident s when is_lower s -> true
        | Ppattern (Parg (l, p)) -> is_ident p
        | _ -> false
      in

      let rec filter s = function
        | [] -> []
        | defn :: q when fast defn = s -> defn :: filter s q
        | defn :: q -> q
      in
      let rec col found i = function
        | [] -> -1
        | (l, x) :: q when is_ident x -> col found (i + 1) q
        | x :: q when found -> raise (MultipleFilteredVariables (fst x))
        | x :: q ->
            let _ = col found (i + 1) q in
            i
      in
      (* Vérification de l'exhaustivité d'un pattern matching *)
      let check_exhaust s = function
        | TArrow (tl, _) -> (
            let l = filter s (snd dinst) in
            match l with
            | [] -> raise (MissingDefinition placeholder_loc)
            | x :: q ->
                let c = col false 0 (sand x) in
                if c = -1 then ()
                else if
                  not
                    (exhaustive_list global_env type_env instance_env
                       [ sub (List.nth tl c) ]
                       (List.map (fun w -> [ Parg (List.nth (sand x) c) ]) l))
                then (raise (NonExhaustivePatternMatching (placeholder_loc))))
        | _ -> failwith "dans check_exhaust, le type récupéré n'est pas TArrow"
      in

      let rec requires = function
        | [] -> []
        | n :: q ->
            ( fst n,
              List.map (ast_atype global_env type_env instance_env) (snd n) )
            :: requires q
      in

      let requirements = requires nl in
      Smaps.iter (fun id t -> check_exhaust id t) class_functions;
      global_env_instances :=
        Smaps.add (fst n)
          (( List.map (ast_atype global_env type_env instance_env) (snd n),
             requirements )
          :: smaps_find (fst n) !global_env_instances)
          !global_env_instances

(* Typage d'un fichier, fonction principale appelée par ppurs.ml *)
and typ_file (f:file) : tfile =
  (* TODO : reconstruire l'AST typé *)
  let global_env = add false "unit" TUnit empty in
  let type_env =
    ref
      (List.fold_left2
         (fun env s t -> Smaps.add s t env)
         Smaps.empty
         [ "Int"; "String"; "Unit"; "Boolean"; "Effect" ]
         [
           (TInt, 0);
           (TStr, 0);
           (TUnit, 0);
           (TBool, 0);
           (TCons ("Effect", [ TUnit ]), 1);
         ])
  in
  let tmain = List.map (typ_declaration global_env type_env !global_env_instances) f.main in
  verify_def global_env !type_env !global_env_instances ; 
  if not (Smaps.mem "main" !function_env) then raise MissingMain ;
  (* programme typé renvoyé *)
  { tmodule_name=f.module_name; tmain }

and placeholder_tdecl = TDefn ("", [], TEatom (TAconst (Cbool true, TBool), TBool))

(* Typage d'une déclaration *)
and typ_declaration global_env (type_env : type_env ref) (instance_env : instance_env) =
  function
  | Dfdecl fdecl ->
      verify_def !global_env_instances !type_env instance_env;
      let tau, var_env, inst_env =
        typ_fdecl global_env !type_env Smaps.empty fdecl
      in
      function_env :=
        Smaps.add fdecl.name (tau, var_env, None, inst_env) !function_env ; 
        placeholder_tdecl
  | Defn defn -> (
      match frst (smaps_find (fast defn) !function_env) with
      | TArrow (tlist, t) ->
          typ_defn global_env !type_env !global_env_instances true defn tlist t ;
          placeholder_tdecl
      | _ -> failwith "dans typ_declaration, le type récupéré n'est pas TArrow")
  | Ddata data ->
      verify_def !global_env_instances !type_env instance_env ;
      typ_data global_env type_env instance_env data ;
      placeholder_tdecl
  | Dclass clas ->
      verify_def !global_env_instances !type_env instance_env ;
      typ_class global_env !type_env instance_env clas ;
      placeholder_tdecl
  | Dinstance (inst, dlist) ->
      verify_def !global_env_instances !type_env instance_env ;
      typ_instance global_env !type_env instance_env (inst, dlist) ;
      placeholder_tdecl