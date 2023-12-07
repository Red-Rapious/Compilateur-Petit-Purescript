/* Analyseur syntaxique */
%{
  open Ast
%}

/* Déclaration des tokens */
%token <Ast.constant> CST
%token <Ast.binop> CMP
%token <Ast.ident> LIDENT
%token <Ast.ident> UIDENT
%token MODULE_MAIN
%token IMPORTS
%token LPAREN RPAREN
%token PLUS MINUS TIMES DIV
%token AND OR
%token /*DOUBLE_EQ*/ SIMPLE_EQ
%token CONCAT
%token LBRACK RBRACK
%token DOUBLE_ARROW SIMPLE_ARROW DOUBLE_POINTS POINT
%token DATA CLASS WHERE INSTANCE
%token IF THEN ELSE DO LET IN CASE OF FORALL 
%token SEMICOLON VBAR COMMA
%token EOF
%token UNITARY_MINUS

/* Priorités et associativités des tokens */
%nonassoc IN ELSE
%left OR 
%left AND
%nonassoc /*DOUBLE_EQ NEQ*/ CMP
%left PLUS MINUS CONCAT
%left TIMES DIV
%nonassoc UNITARY_MINUS

/* Point d'entrée de la grammaire */
%start file

/* Type des valeurs renvoyées par l'analyseur syntaxique */
%type <Ast.file> file
%type <Ast.loc_expr> loc_expr
%type <Ast.loc_atom> loc_atom
%type <Ast.loc_patarg> loc_patarg

%%

lident: id = LIDENT { id };
uident: id = UIDENT { id };

/* Règles de grammaire */
file:
  MODULE_MAIN WHERE LBRACK i=imports SEMICOLON d=separated_nonempty_list(SEMICOLON, decl) RBRACK EOF
    { { main = d } }
;

imports: IMPORTS {};

data_types: id=uident l=list(atype) { (id, l) };

decl:
| d = defn                      { Defn d }
| t = tdecl                     { Dfdecl t }
| DATA 
  name=uident
  params=list(lident) 
  SIMPLE_EQ 
  types=separated_nonempty_list(VBAR, data_types)
{
  Ddata {
    name;
    params;
    types
  }
}
| CLASS name=uident params=list(lident) WHERE LBRACK
    decls=separated_list(SEMICOLON, tdecl)
  RBRACK {
    Dclass {
      name;
      params;
      decls
    }
  }
| INSTANCE inst=instance WHERE LBRACK 
    l=separated_list(SEMICOLON, defn) 
  RBRACK { Dinstance (inst, l) }

;

(*ntype_arrow: n=ntype DOUBLE_ARROW { n };
ntype_arrow_list: ntypes=list(ntype_arrow) { ntypes };
type_arrow: t=typ SIMPLE_ARROW { t };
type_arrow_list: types=list(type_arrow) { types }*)

ntype_arrow_list:
| n=ntype DOUBLE_ARROW { [n] }
| n=ntype DOUBLE_ARROW l=ntype_arrow_list { n::l }
;

type_arrow_list:
| t=typ SIMPLE_ARROW { [t] }
| t=typ SIMPLE_ARROW l=type_arrow_list { t::l }
;

tdecl_variables: FORALL variables=nonempty_list(lident) POINT { variables };
tdecl_end:
  ntypes=option(ntype_arrow_list) types=option(type_arrow_list) (* ICI RÉSIDE LE PROBLÈME *)
  out_type=typ {
    let ntypes = match ntypes with
    | None -> []
    | Some x -> x
    and types = match types with
    | None -> []
    | Some x -> x
    in
    {
      name="";
      variables=[];
      ntypes;
      types;
      out_type
    }
  }


(*tdecl:
  name=lident DOUBLE_POINTS 
  variables=option(tdecl_variables)
  tend=tdecl_end
  { 
    let variables = match variables with
    | Some v -> v
    | None -> []
    in
    {
      name;
      variables;
      ntypes=tend.ntypes;
      types=tend.types;
      out_type=tend.out_type
    }
  }
;*)

tdecl:
  name=lident DOUBLE_POINTS 
  variables=option(tdecl_variables) 
  ntypes=option(ntype_arrow_list)
  types=option(type_arrow_list)
  out_type=typ {
    let variables = match variables with
    | Some v -> v
    | None -> []
    and ntypes = match ntypes with
    | None -> []
    | Some x -> x
    in
    {
      name;
      variables;
      ntypes;
      types=[];
      out_type
    }
  }

defn: name = lident args = list(loc_patarg) SIMPLE_EQ e = loc_expr 
  { (name, args, e) }
;

ntype:
| id=uident l=list(atype) { (id, l) }
;
atype:
| id=uident           { Tident id }
| id=lident           { Tident id }
| LPAREN t=typ RPAREN { Ttype t }
;
typ:
| a=atype                          { Tatype a }
| id=uident l=nonempty_list(atype) { Tntype (id, l) }
(*| n=ntype  { Tntype n }*)
;

instance:
| n=ntype { Intype n }
| n1=ntype DOUBLE_ARROW n2=ntype { Iarrow ([n1], n2)}
| LPAREN l=separated_nonempty_list(COMMA, ntype) RPAREN
  DOUBLE_ARROW n=ntype { Iarrow (l, n) }
;

loc_patarg: p=patarg { ($loc, p) };
patarg: 
| c = CST                       { Pconst c }
| id = lident                   { Pident id }
| id = uident                   { Pident id }
| LPAREN p=pattern RPAREN       { Ppattern p }
;

pattern:
| p = loc_patarg                            { Parg p }
| id = uident l = nonempty_list(loc_patarg) { Pconsarg (id, l) }

loc_atom: a=atom { ($loc, a) };
atom:
| c = CST                                                   { Aconst c }
| id = lident                                               { Aident id }
| id = uident                                               { Aident id }
| LPAREN e=loc_expr RPAREN                                  { Aexpr e }
| LPAREN e=loc_expr DOUBLE_POINTS t=typ RPAREN              { Atypedexpr (e, t) }
;

loc_expr: e=expr { ($loc, e) };
expr:
| a = loc_atom                                              { Eatom a }
| MINUS e = loc_expr %prec UNITARY_MINUS                    { Eunop (Uneg, e) }
| e1 = loc_expr o = binop e2 = loc_expr                     { Ebinop (e1, o, e2) }
| id = lident a = nonempty_list(loc_atom)                   { Efunc (id, a) }
| id = uident a = nonempty_list(loc_atom)                   { Efunc (id, a) }
| IF e1=loc_expr THEN e2=loc_expr ELSE e3=loc_expr          { Eif (e1, e2, e3) }
| DO LBRACK el=separated_nonempty_list(SEMICOLON, loc_expr) RBRACK
                                                { Edo el }
| LET LBRACK b=separated_nonempty_list(SEMICOLON, binding) RBRACK IN e=loc_expr
                                                { Elet (b, e)}
| CASE e=loc_expr OF LBRACK b=separated_nonempty_list(SEMICOLON, branch) RBRACK
                                                { Ecase (e, b) }

binding: id = lident  SIMPLE_EQ   e = loc_expr { (id, e) }
branch:  p = pattern SIMPLE_ARROW e = loc_expr { (p, e) }

%inline binop:
| CONCAT { Bconcat }
| PLUS   { Badd }
| MINUS  { Bsub }
| TIMES  { Bmul }
| DIV    { Bdiv }
| c=CMP  { c    }
| AND    { Band }
| OR     { Bor  }
;