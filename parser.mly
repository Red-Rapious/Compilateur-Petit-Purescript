/* Analyseur syntaxique */

%{
  open Ast
%}

/* Déclaration des tokens */
%token <Ast.constant> CST
%token <Ast.binop> CMP
%token MODULE_MAIN
%token IMPORTS
%token LPAREN RPAREN
%token PLUS MINUS TIMES DIV
%token AND OR
%token EQ NEQ LT LE GT GE 
%token CONCAT
%token LBRACK RBRACK
%token EOF

/* Priorités et associativités des tokens */
%left OR 
%left AND
%nonassoc EQ NEQ LT LE GT GE
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

decl: {};

imports:
  IMPORTS {}
;