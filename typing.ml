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
  type t = TVar

  let compare v1 v2 = Stdlib.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id

  let create =
    let r = ref 0 in
    fun () -> (incr r, { id = !r; def = None })
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
