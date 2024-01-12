open Ast 

let type_of_texpr = function
  | TEatom (_, t) -> t
  | TEunop (_, _, t) -> t
  | TEbinop (_, _, _, t) -> t
  | TEfunc (_, _, t) -> t
  | TEif (_, _, _, t) -> t
  | TEdo (_, t) -> t
  | TElet (_, _, t) -> t
  | TEcase (_, _, t) -> t

let type_of_tatom = function 
  | TAconst(_, t) -> t
  | TAident(_, t) -> t
  | TAexpr(_, t) -> t

let type_of_aexpr = function
| AEatom (_, t, _) -> t
| AEunop (_, _, t, _) -> t
| AEbinop (_, _, _, t, _) -> t
| AEfunc (_, _, t, _) -> t
| AEif (_, _, _, t, _) -> t
| AEdo (_, t, _) -> t
| AElet (_, _, t, _) -> t
| AEcase (_, _, t, _) -> t

let type_of_aatom = function
| AAconst (_, _, t, _) -> t
| AAident (t, _) -> t
| AAexpr (_, t, _) -> t

let address_of_aexpr = function
| AEatom (_, _, a) -> a
| AEunop (_, _, _, a) -> a
| AEbinop (_, _, _, _, a) -> a
| AEfunc (_, _, _, a) -> a
| AEif (_, _, _, _, a) -> a
| AEdo (_, _, a) -> a
| AElet (_, _, _, a) -> a
| AEcase (_, _, _, a) -> a

let address_of_aatom = function
| AAconst (_, _, _, a) -> a
| AAident (_, a) -> a
| AAexpr (_, _, a) -> a