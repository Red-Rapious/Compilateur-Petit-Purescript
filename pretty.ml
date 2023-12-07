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
(*| DOUBLE_EQ -> "== "*)
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

let rec pp_typ fmt = function
  | TArrow ([], t) -> Format.fprintf fmt "%a" pp_atom t
  | TArrow (h :: t, t2) ->
      Format.fprintf fmt "%a ->@ %a" pp_atom h pp_typ (TArrow (t, t2))
  | (TInt | TVar _ | TUnit | TStr | TBool (*| TEffect _*) | TCons _ | TAlias _) as t -> pp_atom fmt t

and pp_atom fmt = function
  | TInt -> Format.fprintf fmt "Int"
  | TUnit -> Format.fprintf fmt "Unit"
  (*| TEffect t ->
      Format.fprintf fmt "Effect ";
      pp_typ fmt t*)
  | TBool -> Format.fprintf fmt "Bool"
  | TStr -> Format.fprintf fmt "String"
  | TVar v -> pp_TVar fmt v
  | TArrow _ as t -> Format.fprintf fmt "@[<1>(%a)@]" pp_typ t
  | _ -> failwith "implémenter le pretty printing de Cons et Alias"

and pp_TVar fmt = function
  | { def = None; id } -> Format.fprintf fmt "'%d" id
  | { def = Some t; id } -> Format.fprintf fmt "@[<1>('%d := %a)@]" id pp_typ t