/* Analyseur syntaxique */

%{
  open Ast
%}

/* Déclaration des tokens */
%token EOF

/* Priorités et associativités des tokens */


/* Point d'entrée de la grammaire */
%start prog

/* Type des valeurs renvoyées par l'analyseur syntaxique */
%type <Ast.program> prog

%%

/* Règles de grammaire */
prog:
  EOF
    { { main = [] } }
;