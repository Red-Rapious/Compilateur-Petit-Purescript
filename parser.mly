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
%token DOUBLE_EQ SIMPLE_EQ
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
%nonassoc DOUBLE_EQ NEQ CMP
%left PLUS MINUS CONCAT
%left TIMES DIV
%nonassoc UNITARY_MINUS

/* Point d'entrée de la grammaire */
%start file

/* Type des valeurs renvoyées par l'analyseur syntaxique */
%type <Ast.file> file

%%

lident: id = LIDENT { id };
uident: id = UIDENT { id };

/* Règles de grammaire */
file:
  MODULE_MAIN i=imports d=separated_nonempty_list(SEMICOLON, decl) EOF
    { { main = d } }
;

imports: IMPORTS {};

data_types: id=uident l=list(atype) { (id, l) };

decl:
| d = defn                      { Defn d }
| t = tdecl                     { Dtdecl t }
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
    defs=separated_list(SEMICOLON, defn)
  RBRACK {
    Dclass {
      name;
      params;
      defs
    }
  }
| INSTANCE inst=instance WHERE LBRACK 
    l=separated_list(SEMICOLON, defn) 
  RBRACK { Dinstance (inst, l) }

;

ntype_arrow: n=ntype DOUBLE_ARROW { n };
type_arrow: t=typ SIMPLE_ARROW { t };
tdecl_variables: FORALL variables=nonempty_list(lident) POINT { variables };

tdecl:
 | name=lident DOUBLE_POINTS 
  variables=option(tdecl_variables) 
  ntypes=list(ntype_arrow) types=list(type_arrow) (* ICI RÉSIDE LE PROBLÈME *)
  out_type=typ
  { 
    let variables = match variables with
    | Some v -> v
    | None -> []
    in
    {
      name;
      variables;
      ntypes;
      types;
      out_type
    }
  }
;

defn: name = lident args = list(patarg) SIMPLE_EQ e = expr 
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

patarg: 
| c = CST                       { Pconst c }
| id = lident                   { Pident id }
| id = uident                   { Pident id }
| LPAREN p=pattern RPAREN       { Ppattern p }
;

pattern:
| p = patarg                    { Parg p }
| id = uident l = nonempty_list(patarg) { Pnamedarg (id, l) }

atom:
| c = CST                                   { Aconst c }
| id = lident                               { Aident id }
| id = uident                               { Aident id }
| LPAREN e = expr RPAREN                    { Aexpr e }
| LPAREN e=expr DOUBLE_POINTS t=typ RPAREN  { Atypedexpr (e, t) }
;

expr:
| a = atom                                      { Eatom a }
| MINUS e = expr %prec UNITARY_MINUS            { Eunop (Uneg, e) }
| e1 = expr o = binop e2 = expr                 { Ebinop (e1, o, e2) }
| id = lident a = nonempty_list(atom)           { Efunc (id, a) }
| id = uident a = nonempty_list(atom)           { Efunc (id, a) }
| IF e1=expr THEN e2=expr ELSE e3=expr          { Eif (e1, e2, e3) }
| DO LBRACK el=separated_nonempty_list(SEMICOLON, expr) RBRACK
                                                { Edo el }
| LET LBRACK b=separated_nonempty_list(SEMICOLON, binding) RBRACK IN e=expr
                                                { Elet (b, e)}
| CASE e=expr OF LBRACK b=separated_nonempty_list(SEMICOLON, branch) RBRACK
                                                { Ecase (e, b) }

binding: id = lident  SIMPLE_EQ    e = expr { (id, e) }
branch:  p = pattern SIMPLE_ARROW e = expr { (p, e) }

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