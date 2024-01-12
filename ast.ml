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

(* AST généré par l'analyse syntaxique *)
type constant = 
| Cbool of bool 
| Cstring of string 
| Cint of int

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

type file = { module_name: string ; main : decl list }

(* Types pour le typage *)
type ttyp =
  | TUnit
  | TBool
  | TInt
  | TStr
  | TArrow of ttyp list * ttyp
  | TVar of tvar
  | TCons of string * ttyp list
  | TAlias of string

and tvar = { id : int; mutable def : ttyp option }

(* AST généré par le typage *)
type texpr =
| TEatom of tatom * ttyp
| TEunop of unop * texpr * ttyp
| TEbinop of texpr * binop * texpr * ttyp
| TEfunc of ident * (tatom list) * ttyp
| TEif of texpr * texpr * texpr * ttyp
| TEdo of texpr list * ttyp
| TElet of tbinding list * texpr * ttyp
| TEcase of texpr * (tbranch list) * ttyp
and tatom =
| TAconst of constant * ttyp
| TAident of ident * ttyp
| TAexpr of texpr * ttyp


and tbranch = pattern * texpr 
(* On conserve le type pattern précédent puisqu'on utilise pas le type des patterns *)

and tbinding = ident * texpr

and tfdecl = {
  tname: ident;
  tvariables: ident list;
  tntypes: ntype list;
  ttypes: ttyp list;
  tout_type: ttyp
}


and tpattern = 
| TParg of patarg
| TPconsarg of ident * (patarg list)


type tdefn = ident * patarg list * texpr

type tdecl = 
| TDefn of tdefn
(* 
  les lignes ci-dessous sont peut-être à ajuster pour des versions
  typées de data, class, instance
*)
| TDfdecl of tfdecl
| TDdata of data
| TDclass of clas
| TDinstance of instance * (tdefn list)

type tfile = { tmodule_name: string ; tmain : tdecl list }

(* AST généré après l'allocation des variables *)
type frame_size = int

(* le dernier entier stocke l'adresse du résultat du calcul *)
type aexpr =
| AEatom of aatom * ttyp * int
| AEunop of unop * aexpr * ttyp * int
| AEbinop of aexpr * binop * aexpr * ttyp * int
| AEfunc of ident * (aatom list) * ttyp * int
| AEif of aexpr * aexpr * aexpr * ttyp * int
| AEdo of aexpr list * ttyp * int
| AElet of abinding list * aexpr * ttyp * int
| AEcase of aexpr * (abranch list) * ttyp * int
and aatom =
| AAconst of constant * int * ttyp * int
| AAident of ttyp * int(* adresse, type *)
| AAexpr of aexpr * ttyp * int

and abranch = pattern * aexpr

and abinding = int * aexpr

and afdecl = {
  aname: ident;
  avariables: ident list;
  antypes: ntype list;
  atypes: ttyp list;
  aout_type: ttyp
}

and apatarg = 
| APconst of constant * int
| APident of ident * int

type adefn = ident * apatarg list * aexpr * frame_size

type adecl = 
| ADefn of adefn
(* 
  les lignes ci-dessous sont peut-être à ajuster pour des versions
  custom de data, class, instance
*)
| ADfdecl of afdecl
| ADdata of data
| ADclass of clas
| ADinstance of instance * (adefn list)