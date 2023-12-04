open Ast

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

module Smap = Map.Make (Int)

type env = ttyp Smap.t

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
  | (TInt | TBool | TStr | TEffect _ | TUnit) as t -> t
  | TVar { id = a; def = None } -> TVar { id = a; def = None }
  | TVar { id = a; def = Some t } -> TVar { id = a; def = Some (canon t) }
  | TArrow (t1, t2) -> TArrow (List.map canon t1, canon t2)

let rec pp_typ fmt = function
  | TArrow ([], t) -> Format.fprintf fmt "%a" pp_atom t
  | TArrow (h :: t, t2) ->
      Format.fprintf fmt "%a ->@ %a" pp_atom h pp_typ (TArrow (t, t2))
  | (TInt | TVar _ | TUnit | TStr | TBool | TEffect _) as t -> pp_atom fmt t

and pp_atom fmt = function
  | TInt -> Format.fprintf fmt "Int"
  | TUnit -> Format.fprintf fmt "Unit"
  | TEffect t ->
      Format.fprintf fmt "Effect ";
      pp_typ fmt t
  | TBool -> Format.fprintf fmt "Bool"
  | TStr -> Format.fprintf fmt "String"
  | TVar v -> pp_TVar fmt v
  | TArrow _ as t -> Format.fprintf fmt "@[<1>(%a)@]" pp_typ t

and pp_TVar fmt = function
  | { def = None; id } -> Format.fprintf fmt "'%d" id
  | { def = Some t; id } -> Format.fprintf fmt "@[<1>('%d := %a)@]" id pp_typ t

exception UnificationFailure of ttyp * ttyp

let unification_error t1 t2 = raise (UnificationFailure (canon t1, canon t2))

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

module Smap = Map.Make (String)

type env = { bindings : schema Smap.t; fvars : Vset.t }

let empty = { bindings = Smap.empty; fvars = Vset.empty }

let add gen x t env =
  let vt = fvars t in
  let s, fvars =
    if gen then
      let env_fvars = norm_varset env.fvars in
      ({ vars = Vset.diff vt env_fvars; ttyp = t }, env.fvars)
    else ({ vars = Vset.empty; ttyp = t }, Vset.union env.fvars vt)
  in
  { bindings = Smap.add x s env.bindings; fvars }

module Vmap = Map.Make (V)

let find x env =
  let tx = Smap.find x env.bindings in
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

let rec ast_type env newtypes = function
  | Tatype a -> ast_atype env newtypes a
  | Tntype n -> ast_ntype env newtypes n

and ast_atype env newtypes = function
  | Tident "String" -> TStr
  | Tident "Int" -> TInt
  | Tident "Boolean" -> TBool
  | Tident "Unit" -> TUnit
  | Ttype t -> ast_type env newtypes t
  | _ -> TBool

and ast_ntype env newtypes = function
  (*Pakomprienfét*)
  | id, h :: t -> TInt
  | id, [] -> TBool

let rec typ_exp env newtypes expr =
  match expr with
  | Eunop (_, e) -> (
      match typ_exp env newtypes e with
      | TInt -> TInt
      | _ -> failwith "Erreur : Implémenter la localisation")
  | Eif (e1, e2, e3) -> (
      match typ_exp env newtypes e1 with
      | TBool ->
          let s2 = typ_exp env newtypes e2 in
          let s3 = typ_exp env newtypes e3 in
          if s2 == s3 then s2
          else failwith "Erreur : Implémenter la localisation"
      | _ -> failwith "Erreur : Implémenter la localisation")
  | Ebinop (e1, op, e2) -> (
      match op with
      | Beq | Bneq -> (
          let s1 = typ_exp env newtypes e1 in
          let s2 = typ_exp env newtypes e2 in
          match (s1, s2) with
          | TInt, TInt | TBool, TBool | TStr, TStr -> TBool
          | _ -> failwith "Erreur : Implémenter la localisation")
      | Blt | Ble | Bgt | Bge -> (
          let s1 = typ_exp env newtypes e1 in
          let s2 = typ_exp env newtypes e2 in
          match (s1, s2) with
          | TInt, TInt -> TBool
          | _ -> failwith "Erreur : Implémenter la localisation")
      | Bsub | Badd | Bmul | Bdiv -> (
          let s1 = typ_exp env newtypes e1 in
          let s2 = typ_exp env newtypes e2 in
          match (s1, s2) with
          | TInt, TInt -> TInt
          | _ -> failwith "Erreur : Implémenter la localisation")
      | Bconcat -> (
          let s1 = typ_exp env newtypes e1 in
          let s2 = typ_exp env newtypes e2 in
          match (s1, s2) with
          | TStr, TStr -> TStr
          | _ -> failwith "Erreur : Implémenter la localisation")
      | Band | Bor -> (
          let s1 = typ_exp env newtypes e1 in
          let s2 = typ_exp env newtypes e2 in
          match (s1, s2) with
          | TBool, TBool -> TBool
          | _ -> failwith "Erreur : Implémenter la localisation"))
  | Eatom a -> TInt
  | Edo l ->
      if
        List.for_all
          (fun e -> typ_eq (typ_exp env newtypes e) (TEffect TUnit))
          l
      then TEffect TUnit
      else failwith "Erreur : Implémenter la localisation"
  | Ecase (e, []) -> failwith "Empty Pattern Matching bro"
  | Ecase (e, l) ->
      let t = typ_exp env newtypes e in
      let rettypes = List.map (fun x -> typ_branch env newtypes x) l in
      let tret = snd (List.hd rettypes) in
      if
        List.for_all (fun x -> typ_eq t (fst x)) rettypes
        && List.for_all (fun x -> typ_eq tret (snd x)) rettypes
      then tret
      else failwith "Erreur : Implémenter la Localisation"
  | Elet (bl, e) ->
      typ_exp
        (List.fold_left (fun env x -> typ_binding env newtypes x) env bl)
        newtypes e
  | _ -> TInt

and typ_atom env ntypes = function
  | Aconst c -> (
      match c with Cbool _ -> TBool | Cint _ -> TInt | Cstring _ -> TStr)
  | Aident id -> (
      try find id env
      with Not_found -> failwith "Erreur : Implémenter la localisation")
  | Aexpr e -> typ_exp env ntypes e
  | Atypedexpr (e, tau) ->
      let t = ast_type env ntypes tau in
      if typ_eq t (typ_exp env ntypes e) then t
      else failwith "Erreur : Implémenter la localisation"

and typ_branch env ntypes = function
  | p, e -> (typ_pattern env ntypes p, typ_exp env ntypes e)
  | _ ->
      (*Ceci ne sera jamais affiché si le lexing et le parsing sont corrects*)
      failwith
        "You think I hate you? I don't hate you. This job is eating me alive, \
         I can't breathe anymore, and if I come out this guy Lefty dies. \
         They're gonna kill him because he vouched for me because he stood up \
         for me I live with that every day. That's the same thing as if I put \
         the bullet in his head myself, you understand? I spent all these \
         years being the good guy, you know the man in the white fucking hat, \
         for what? For nothing. I'm not becoming like them Maggie, I am them."

and typ_pattern env ntypes = function
  | Parg p -> typ_patarg env ntypes p
  | Pnamedarg (id, l) -> TBool (*Pas compris encore*)

and typ_patarg env ntypes = function
  | Pconst c -> (
      match c with Cbool _ -> TBool | Cint _ -> TInt | Cstring _ -> TStr)
  | Pident id -> (
      try find id env
      with Not_found -> failwith "Erreur : Implémenter la Localisation")
  | Ppattern p -> typ_pattern env ntypes p

and typ_binding env newtypes = function
  | id, e -> add true id (typ_exp env newtypes e) env


let rec well_formed_declaration env newtypes declaration = 
  let table = Hashtbl.create (List.length declaration.variables) in
  let rec run = function
    | h::t -> Hashtbl.add table h h; run t
    | [] -> ()
  in run declaration.variables;
  List.for_all (fun i -> well_formed_instance env newtypes (Intype i  )) declaration.ntypes && List.for_all (fun i-> well_formed_typ env newtypes i) declaration.types 

and well_formed_typ = failwith "todo"

and well_formed_instance env newtypes = function
  | Intype n ->  true
  | _ -> false
  
  