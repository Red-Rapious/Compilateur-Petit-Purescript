open Ast 

let type_of_aexpr = function
| AEatom (_, t, _) -> t
| AEunop (_, _, t, _) -> t
| AEbinop (_, _, _, t, _) -> t
| AEfunc _ -> failwith "get type of efunc"
| AEif (_, _, _, t, _) -> t
| AEdo (_, t, _) -> t
| AElet (_, _, t, _) -> t
| AEcase (_, _, t, _) -> t

let address_of_aexpr = function
| AEatom (_, _, a) -> a
| AEunop (_, _, _, a) -> a
| AEbinop (_, _, _, _, a) -> a
| AEfunc (_, _, a) -> a
| AEif (_, _, _, _, a) -> a
| AEdo (_, _, a) -> a
| AElet (_, _, _, a) -> a
| AEcase (_, _, _, a) -> a

let address_of_aatom = function
| AAconst (_, _, a) -> a
| AAident (_, a) -> a
| AAexpr (_, _, a) -> a