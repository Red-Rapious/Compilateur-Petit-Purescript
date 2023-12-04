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

type atype = 
| Tident of ident
| Ttype of typ 

and ntype = ident * (atype list)

and typ = 
| Tatype of atype
| Tntype of ntype

type instance =
| Intype of ntype
| Iarrow of (ntype list) * ntype

type patarg = 
| Pconst of constant 
| Pident of ident
| Ppattern of pattern

and pattern = 
| Parg of patarg
| Pnamedarg of ident * (patarg list)

type expr =
| Eatom of atom
| Eunop of unop * expr
| Ebinop of expr * binop * expr
| Efunc of ident * (atom list)
| Eif of expr * expr * expr
| Edo of expr list
| Elet of  binding list * expr
| Ecase of expr * (branch list)

and atom = 
| Aconst of constant 
| Aident of ident 
| Aexpr of expr
| Atypedexpr of expr * typ

and branch = pattern * expr

and binding = ident * expr

type defn = ident * patarg list * expr

type decl = 
| Defn of defn
| Dtdecl of tdecl
| Ddata of data
| Dclass of clas
| Dinstance of instance * (defn list)

and tdecl = {
  name: ident;
  variables: ident list;
  ntypes: ntype list;
  types: typ list;
  out_type: typ
}

and data = {
  name: ident;
  params: ident list;
  types: (ident * (atype list)) list
}

and clas = { 
  name: ident; 
  params: ident list; 
  defs: defn list 
}

type file = { main : decl list }
