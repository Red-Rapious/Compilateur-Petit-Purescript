/* Analyseur syntaxique */

%{
  open Ast
%}

/* Déclaration des tokens */
%token <Ast.constant> CST
%token <Ast.binop> CMP
%token <Ast.ident> IDENT
%token MODULE_MAIN
%token IMPORTS
%token LPAREN RPAREN
%token PLUS MINUS TIMES DIV
%token AND OR
%token DOUBLE_EQ NEQ LT LE GT GE 
%token SIMPLE_EQ
%token CONCAT
%token LBRACK RBRACK
%token EOF

/* Priorités et associativités des tokens */
%left OR 
%left AND
%nonassoc DOUBLE_EQ NEQ LT LE GT GE
%left PLUS MINUS CONCAT
%left TIMES DIV
%nonassoc unitary_minus

/* Point d'entrée de la grammaire */
%start file

/* Type des valeurs renvoyées par l'analyseur syntaxique */
%type <Ast.program> file

%%

/* Règles de grammaire */
file:
  MODULE_MAIN i=imports d=decl EOF
    { { main = [] } }
;

atom:
| e = expr        { Aexpr e }
| id = ident      { Aident id }
;

expr:
| c = CST         { Econst c }

defn: name = ident args = list(patarg) SIMPLE_EQ e = expr 
  { (name, args, e) }
;

patarg: 
| c = CST         { Pconst c }
| id = ident      { Pident id }
;

decl:
| d = defn        { Defn d }
;

imports:
  IMPORTS {}
;

%inline binop:
| PLUS  { Badd }
| MINUS { Bsub }
| TIMES { Bmul }
| DIV   { Bdiv }
| c=CMP { c    }
| AND   { Band }
| OR    { Bor  }
;

ident:
  id = IDENT { id }
;