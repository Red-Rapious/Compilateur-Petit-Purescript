open Ast
open Format
open Parser

let print_binop b = 
Format.printf "%s" (match b with
| Beq -> "=="
| Bneq -> "!="
| Blt ->  "<"
| Ble -> "<="
| Bgt -> ">"
| Bge -> ">="
| Badd -> "+"
| Bsub -> "-"
| Bmul -> "*"
| Bdiv -> "/"
| Band -> "&&"
| Bor -> "||"
| Bconcat -> "<>"
)

let print_token t = match t with
(* %token <Ast.constant> CST
%token <Ast.binop> CMP
%token <Ast.ident> LIDENT
%token <Ast.ident> UIDENT *)
| MODULE_MAIN -> "module Main where\n"
| IMPORTS -> "imports\n"
| LPAREN -> "("
| RPAREN -> ")"
| PLUS -> "+ "
| MINUS -> "- "
| TIMES -> "* "
| DIV -> "/ "
| AND -> "&& "
| OR -> "|| "
| DOUBLE_EQ -> "== "
| NEQ -> "!= "
| SIMPLE_EQ -> "= "
| CONCAT -> "<> "
| LBRACK -> "{ "
| RBRACK -> "} "
| DOUBLE_ARROW -> "=> "
| SIMPLE_ARROW -> "->"
| DOUBLE_POINTS -> ":: "
| DATA -> "data"
| CLASS -> "class "
| WHERE -> "where "
| INSTANCE -> "instance "
| IF -> "if "
| THEN -> "then "
| ELSE -> "else "
| DO -> "do "
| LET -> "let "
| IN -> "in "
| CASE -> "case "
| OF -> "of "
| FORALL -> "forall "
| SEMICOLON -> "; "
| VBAR -> "| "
| EOF -> "EOF\n"
| UNITARY_MINUS -> "u- "
| LIDENT i -> i ^ " "
| UIDENT i -> i ^ " "
| _ -> "?? "