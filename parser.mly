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
%token DOUBLE_ARROW SIMPLE_ARROW DOUBLE_POINTS
%token DATA CLASS WHERE INSTANCE
%token IF THEN ELSE DO LET IN CASE OF FORALL 
%token SEMICOLON
%token EOF

/* Priorités et associativités des tokens */
%left OR 
%left AND
%nonassoc DOUBLE_EQ NEQ LT LE GT GE LPAREN RPAREN LBRACK RBRACK
%left PLUS MINUS CONCAT
%left TIMES DIV
%nonassoc unitary_minus

/* Point d'entrée de la grammaire */
%start file

/* Type des valeurs renvoyées par l'analyseur syntaxique */
%type <Ast.file> file

%%

/* Règles de grammaire */
file:
  MODULE_MAIN i=imports d=separated_list(SEMICOLON, decl) EOF
    { { main = d } }
;

atom:
| c = CST                       { Aconst c }
| id = ident                    { Aident id }
| LPAREN e = expr RPAREN        { Aexpr e }
;

expr:
| a = atom                                      { Eatom a }
| MINUS e1 = expr %prec unitary_minus           { Eunop (Uneg, e1) }
| e1 = expr o = binop e2 = expr                 { Ebinop (e1, o, e2) }
| id = ident a = nonempty_list(atom)            { Efunc (id, a) }
| IF e1=expr THEN e2=expr ELSE e3=expr          { Eif (e1, e2, e3) }
| DO LBRACK el=nonempty_list(expr) RBRACK       { Edo el }
| LET LBRACK b=separated_list(SEMICOLON, binding) RBRACK IN e=expr
                                                { Elet (b, e)}
| CASE e=expr OF LBRACK b=separated_list(SEMICOLON, branch) RBRACK
                                                { Ecase (e, b) }

decl:
| d = defn                      { Defn d }
;

defn: name = ident args = list(patarg) SIMPLE_EQ e = expr 
  { (name, args, e) }
;

patarg: 
| c = CST                       { Pconst c }
| id = ident                    { Pident id }
| LPAREN p=pattern RPAREN       { Ppattern p }
;

imports:
  IMPORTS {}
;

pattern:
| p = patarg                    { Parg p }
| id = ident l = nonempty_list(patarg) { Pnamedarg (id, l) }

binding: id = ident  SIMPLE_EQ    e = expr { (id, e) }
branch:  p = pattern SIMPLE_ARROW e = expr { (p, e) }

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