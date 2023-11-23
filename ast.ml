type ident = string

type binop =
  | Beq
  | Bneq
  | Blt
  | Ble
  | Bgt
  | Bge
  | Badd
  | Bsub
  | Bmul
  | Bdiv
  | Band
  | Bor
  | Bconcat

type unop = Uneg
type constant = Cbool of bool | Cstring of string | Cint of int
type patarg = Pconst of constant | Pident of ident
(*| Ppattern of pattern*)

type expr =
  | Econst of constant
  | Ebinop of expr * binop * expr
  | Eunop of unop * expr
  | Eatom of atom
  | Efunc of ident * (atom list)

and atom = Aconst of constant | Aident of ident | Aexpr of expr
(*| Atypedexpr of expr * typ*)

type defn = ident * patarg list * expr
type decl = Defn of defn
type program = { main : decl list }
