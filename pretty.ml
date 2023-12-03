open Ast
open Format
open Parser

let print_binop b = 
match b with
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

let print_token t = match t with
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
(*| NEQ -> "!= "*)
| POINT -> ". "
| COMMA -> ", "
| SIMPLE_EQ -> "= "
| CONCAT -> "<> "
| LBRACK -> "{\n\t"
| RBRACK -> "}\n"
| DOUBLE_ARROW -> "=> "
| SIMPLE_ARROW -> "-> "
| DOUBLE_POINTS -> ":: "
| DATA -> "data "
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
| SEMICOLON -> ";\n\t"
| VBAR -> "| "
| EOF -> "EOF\n"
| UNITARY_MINUS -> "u- "
| LIDENT i -> i ^ " "
| UIDENT i -> i ^ " "
| CST c -> 
  (match c with
  | Cbool b -> string_of_bool b ^ " "
  | Cstring s -> "\"" ^ s ^ "\" "
  | Cint i -> string_of_int i ^ " "
  )
| CMP binop -> print_binop binop