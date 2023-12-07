type loc = Lexing.position * Lexing.position

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
and loc_patarg = loc * patarg

and pattern = 
| Parg of loc_patarg
| Pconsarg of ident * (loc_patarg list)

type expr =
| Eatom of loc_atom
| Eunop of unop * loc_expr
| Ebinop of loc_expr * binop * loc_expr
| Efunc of ident * (loc_atom list)
| Eif of loc_expr * loc_expr * loc_expr
| Edo of loc_expr list
| Elet of  binding list * loc_expr
| Ecase of loc_expr * (branch list)
and loc_expr = loc * expr

and atom = 
| Aconst of constant 
| Aident of ident 
| Aexpr of loc_expr
| Atypedexpr of loc_expr * typ
and loc_atom = loc * atom

and branch = pattern * loc_expr

and binding = ident * loc_expr

type defn = ident * loc_patarg list * loc_expr

type decl = 
| Defn of defn
| Dfdecl of fdecl
| Ddata of data
| Dclass of clas
| Dinstance of instance * (defn list) (*C*)

and fdecl = {
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
  decls: fdecl list
}

type file = { main : decl list }

type ttyp =
  | TUnit
  | TBool
  | TInt
  | TStr
  | TEffect of ttyp
  | TArrow of ttyp list * ttyp
  | TVar of tvar
  | TCons of ttyp list

and tvar = { id : int; mutable def : ttyp option }