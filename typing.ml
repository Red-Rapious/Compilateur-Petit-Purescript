open Ast

type typ =
  | TUnit
  | TBool
  | TInt
  | TStr
  | TEffectUnit
  | TArrow of typ * typ
  | TProduct of typ * typ
  | TVar of tvar

and tvar = { id : int; mutable def : typ option }

module Smap = Map.Make (Int)

type env = typ Smap.t

module V = struct
  type t = tvar

  let compare v1 v2 = Stdlib.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id

  let create =
    let r = ref 0 in
    fun () -> (incr r; { id = !r; def = None })
end

let rec head = function TVar { def = Some t } -> head t | t -> t

let rec canon t =
  match head t with
  | (TVar _ | TInt | TBool | TStr | TEffectUnit | TUnit) as t -> t
  | TArrow (t1, t2) -> TArrow (canon t1, canon t2)
  | TProduct (t1, t2) -> TProduct (canon t1, canon t2)

let rec pp_typ fmt = function
  | TProduct (t1, t2) -> Format.fprintf fmt "%a *@ %a" pp_atom t1 pp_atom t2
  | TArrow (t1, t2) -> Format.fprintf fmt "%a ->@ %a" pp_atom t1 pp_typ t2
  | (TInt | TVar _ | TUnit | TStr | TBool | TEffectUnit) as t -> pp_atom fmt t

and pp_atom fmt = function
  | TInt -> Format.fprintf fmt "Int"
  | TUnit -> Format.fprintf fmt "Unit"
  | TEffectUnit -> Format.fprintf fmt "Effect Unit"
  | TBool -> Format.fprintf fmt "Bool"
  | TStr -> Format.fprintf fmt "String"
  | TVar v -> pp_TVar fmt v
  | (TArrow _ | TProduct _) as t -> Format.fprintf fmt "@[<1>(%a)@]" pp_typ t

and pp_TVar fmt = function
  | { def = None; id } -> Format.fprintf fmt "'%d" id
  | { def = Some t; id } -> Format.fprintf fmt "@[<1>('%d := %a)@]" id pp_typ t

exception UnificationFailure of typ * typ

let unification_error t1 t2 = raise (UnificationFailure (canon t1, canon t2))

let rec occur v t =
  match head t with
  | TVar w -> V.equal v w
  | TArrow (t1, t2) | TProduct (t1, t2) -> occur v t1 || occur v t2
  | _ -> false

let rec unify t1 t2 =
  match (head t1, head t2) with
  | TVar v1, TVar v2 when V.equal v1 v2 -> ()
  | (TVar v1 as t1), t2 ->
      if occur v1 t2 then unification_error t1 t2;
      assert (v1.def = None);
      v1.def <- Some t2
  | t1, TVar v2 -> unify t2 t1
  | TArrow (t11, t12), TArrow (t21, t22)
  | TProduct (t11, t12), TProduct (t21, t22) ->
      unify t11 t21;
      unify t12 t22
  | TInt, TInt
  | TBool, TBool
  | TStr, TStr
  | TUnit, TUnit
  | TEffectUnit, TEffectUnit ->
      ()
  | t1, t2 -> unification_error t1 t2

module Vset = Set.Make(V)
let rec fvars t = match head t with
  | TInt | TStr | TBool | TUnit | TEffectUnit -> Vset.empty
  | TArrow (t1, t2) | TProduct (t1, t2) -> Vset.union (fvars t1) (fvars t2)
  | TVar v -> Vset.singleton v

let norm_varset s =
  Vset.fold (fun v s -> Vset.union (fvars (TVar v)) s) s Vset.empty


type schema = { vars : Vset.t; typ : typ }
module Smap = Map.Make(String)

type env = { bindings : schema Smap.t; fvars : Vset.t }

let empty = { bindings = Smap.empty; fvars = Vset.empty }

let add gen x t env =
  let vt = fvars t in
  let s, fvars =
    if gen then
      let env_fvars = norm_varset env.fvars in
      { vars = Vset.diff vt env_fvars; typ = t }, env.fvars
    else
      { vars = Vset.empty; typ = t }, Vset.union env.fvars vt
  in
  { bindings = Smap.add x s env.bindings; fvars = fvars }

module Vmap = Map.Make(V)


let find x env =
  let tx = Smap.find x env.bindings in
  let s =
    Vset.fold (fun v s -> Vmap.add v (TVar (V.create ())) s)
      tx.vars Vmap.empty
  in
  let rec subst t = match head t with
    | TVar x as t -> (try Vmap.find x s with Not_found -> t)
    | TInt | TBool | TStr | TUnit | TEffectUnit as t -> t
    | TArrow (t1, t2) -> TArrow (subst t1, subst t2)
    | TProduct (t1, t2) -> TProduct (subst t1, subst t2)
  in
  subst tx.typ