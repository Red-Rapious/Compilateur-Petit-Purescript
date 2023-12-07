(*
type file =
  {imports : imports; decls : decl list}

and imports = Import

and decl =
  | Ddefn of defn
  | Dtdecl of tdecl
  | Ddata of ident * (ident list) * ((ident * (atype list)) list)
  | Dclass of ident * (ident list) * tdecl list
  | Dinstance of instance * defn list

and defn =
  {ident : ident; patargs : patarg list; expr : expr}

and tdecl =
  {dident : ident; identlist : ident list; ntypelist : ntype list; purtypelist : purtype list; purtype : purtype}

and ntype =
  {nident : ident; atypes : atype list}

and atype =
  | Aident of ident
  | Apurtype of purtype

and purtype =     (*remplace type car type est un mot clef en Ocaml*)
  | Patype of atype
  | Pntype of ntype

and instance =
  | Intype of ntype
  | Iarrow of ntype * ntype
  | Imularrow of ntype list * ntype

and patarg =
  | Pconstant of constant
  | Plident of ident
  | Puident of ident
  | Ppattern of pattern

and pattern =
  | Ppatarg of patarg
  | Pmulpatarg of ident * patarg list

and constant =
  | Cbool of bool
  | Cint of int
  | Cstring of string

and atom =
  | Aconstant of constant
  | Alident of ident
  | Auident of ident
  | Aexpr of expr
  | Aexprtype of expr * purtype

and expr =
  | Eatom of atom
  | Ebinop of binop * expr * expr
  | Elident of ident * atom list 
  | Euident of ident * atom list 
  | Eif of expr * expr * expr
  | Edo of expr list
  | Elet of binding list * expr
  | Ecase of expr * branch list

and binding =
  {ident : ident; bindexpr : expr}

and branch =
  {pattern : pattern; expr : expr}

and binop = Bequals | Bnotequals | Binf | Binfeq | Bsup | Bsupeq | Bplus | Bminus | Btimes | Bdivide | Band | Bor | Bcons


and ident = string
*)