open Ast
open Format
open Parser

let reset_code = "\o033[0m"
let blue_code = "\o033[34m"
let red_code = "\o033[91m"
let green_code = "\o033[92m"
let magenta_code = "\o033[95m"
let yellow_code = "\o033[93m"

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
| MODULE -> "module "
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

and pp_atom fmt a = 
Format.fprintf fmt "%s" magenta_code ;
begin 
match a with
| TInt -> Format.fprintf fmt "Int"
| TUnit -> Format.fprintf fmt "Unit"
(*| TEffect t ->
    Format.fprintf fmt "Effect ";
    pp_typ fmt t*)
| TBool -> Format.fprintf fmt "Bool"
| TStr -> Format.fprintf fmt "String"
| TVar v -> pp_TVar fmt v
| TArrow _ as t -> Format.fprintf fmt "@[<1>(%a)@]" pp_typ t
| TAlias tag -> Format.fprintf fmt "Alias %s" tag
| TCons (tag, tlist) -> 
  Format.fprintf fmt "Cons (%s, [" tag ;
  List.iter (fun t -> 
    pp_atom fmt t ;
    Format.fprintf fmt "%s,%s " magenta_code reset_code (* oui, même pour le dernier *)
  ) tlist ;
  Format.fprintf fmt "%s])%s" magenta_code reset_code
end
;
Format.fprintf fmt "%s" reset_code


and pp_TVar fmt = function
  | { def = None; id } -> Format.fprintf fmt "'%d" id
  | { def = Some t; id } -> Format.fprintf fmt "@[<1>('%d := %a)@]" id pp_typ t

let ident fmt depth =
  for _ = 1 to depth do 
    Format.fprintf fmt "\t"
  done

let rec pp_aexpr fmt depth = function
| AEatom (a, t, i) -> 
  ident fmt depth ;
  Format.fprintf fmt "%sAEatom%s of type " blue_code reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt " with offset %s%d%s@." yellow_code i reset_code ;
  pp_aatom fmt (depth + 1) a
| AEunop (_, e, t, i) ->
  ident fmt depth ;
  Format.fprintf fmt "%sAEunop%s of type " blue_code reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt " with offset %s%d%s@." yellow_code i reset_code ;
  pp_aexpr fmt (depth + 1) e
| AEbinop (e1, b, e2, t, i) ->
  ident fmt depth ;
  Format.fprintf fmt "%sAEbinop \'%s\'%s of type " blue_code (print_binop b) reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt " with offset %s%d%s@." yellow_code i reset_code ;
  ident fmt depth ;
  Format.fprintf fmt "First expression:@." ;
  pp_aexpr fmt (depth + 1) e1 ;
  ident fmt depth ;
  Format.fprintf fmt "Second expression:@." ;
  pp_aexpr fmt (depth + 1) e2
| AEfunc (id, alist, t, i) ->
  ident fmt depth ;
  Format.fprintf fmt "%sAEfunc%s named %s of type " blue_code reset_code id;
  pp_typ fmt t ;
  Format.fprintf fmt " with offset %s%d%s@." yellow_code i reset_code ;
  ident fmt depth ;
  Format.fprintf fmt "List of contained atoms:@." ;
  List.iter (pp_aatom fmt (depth + 1)) alist
| AEif (e1, e2, e3, t, i) ->
  ident fmt depth ;
  Format.fprintf fmt "%sAEif%s of type " blue_code reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt " with offset %s%d%s@." yellow_code i reset_code ;
  ident fmt depth ;
  Format.fprintf fmt "Condition:@." ;
  pp_aexpr fmt (depth + 1) e1 ;
  ident fmt depth ;
  Format.fprintf fmt "then:@." ;
  pp_aexpr fmt (depth + 1) e2 ;
  ident fmt depth ;
  Format.fprintf fmt "else:@." ;
  pp_aexpr fmt (depth + 1) e3
| AEdo (elist, t, i) ->
  ident fmt depth ;
  Format.fprintf fmt "%sAEdo%s of type " blue_code reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt " with offset %s%d%s@." yellow_code i reset_code ;
  ident fmt depth ;
  Format.fprintf fmt "List of contained expressions:@." ;
  List.iter (pp_aexpr fmt (depth + 1)) elist
| _ ->   ident fmt depth ; Format.fprintf fmt "%sTHIS PART IS NOT SUPPORTED BY THE PRETTY PRINTER%s" red_code reset_code
and pp_aatom fmt depth a = 
ident fmt depth ;
match a with
| AAconst (c, i, t, i') ->
  Format.fprintf fmt "%sAAconst%s of type " blue_code reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt " with const offset %s%d%s and result offset of %s%d%s@." yellow_code i reset_code yellow_code i' reset_code;
  pp_const fmt (depth + 1) c
| AAuident (_, t, i) -> 
  Format.fprintf fmt "%sAAuident%s of type " blue_code reset_code;
  pp_typ fmt t ;
  Format.fprintf fmt " with offset %s%d%s@." yellow_code i reset_code ;
| AAlident (t, i) -> 
  Format.fprintf fmt "%sAAident%s of type " blue_code reset_code;
  pp_typ fmt t ;
    Format.fprintf fmt " with offset %s%d%s@." yellow_code i reset_code ;
| AAexpr (e, t, i) -> 
  Format.fprintf fmt "%sAAexpr%s of type " blue_code reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt " with offset %s%d%s@." yellow_code i reset_code ;
  pp_aexpr fmt (depth + 1) e 
and pp_const fmt depth c = 
ident fmt depth ;
match c with 
| Cbool b -> Format.fprintf fmt "Cbool %s%b%s@." green_code b reset_code
| Cstring s -> Format.fprintf fmt "Cstring %s\"%s\"%s@." green_code s reset_code
| Cint i -> Format.fprintf fmt "Cint %s%d%s@." green_code i reset_code