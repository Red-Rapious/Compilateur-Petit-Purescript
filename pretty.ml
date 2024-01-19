open Ast
open Format
open Parser

module Smap = Map.Make(String)

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
| TArrow ([], t) -> Format.fprintf fmt "%a" pp_ttyp t
| TArrow (h :: t, t2) ->
    Format.fprintf fmt "%a ->@ %a" pp_ttyp h pp_typ (TArrow (t, t2))
| (TInt | TVar _ | TUnit | TStr | TBool (*| TEffect _*) | TCons _ | TAlias _) as t -> pp_ttyp fmt t

and pp_ttyp fmt a = 
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
    pp_ttyp fmt t ;
    Format.fprintf fmt "%s,%s " magenta_code reset_code (* oui, même pour le dernier *)
  ) tlist ;
  Format.fprintf fmt "%s])%s" magenta_code reset_code
end
;
Format.fprintf fmt "%s" reset_code


and pp_TVar fmt = function
  | { def = None; id } -> Format.fprintf fmt "'%d" id
  | { def = Some t; id } -> Format.fprintf fmt "@[<1>('%d := %a)@]" id pp_typ t

let indent fmt depth =
  for _ = 1 to depth do 
    Format.fprintf fmt "\t"
  done

let pp_const fmt depth c = 
  indent fmt depth ;
  match c with 
  | Cbool b -> Format.fprintf fmt "%sCbool%s %s%b%s@." blue_code reset_code green_code b reset_code
  | Cstring s -> Format.fprintf fmt "%sCstring%s %s\"%s\"%s@." blue_code reset_code green_code s reset_code
  | Cint i -> Format.fprintf fmt "%sCint%s %s%d%s@." blue_code reset_code green_code i reset_code
  


let rec pp_texpr fmt depth = function
| TEatom (a, t) -> 
  indent fmt depth ;
  Format.fprintf fmt "%sTEatom%s of type " blue_code reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt "@." ;
  pp_tatom fmt (depth + 1) a
| TEunop (_, e, t) ->
  indent fmt depth ;
  Format.fprintf fmt "%sTEunop%s of type " blue_code reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt "@." ;
  pp_texpr fmt (depth + 1) e
| TEbinop (e1, b, e2, t) ->
  indent fmt depth ;
  Format.fprintf fmt "%sTEbinop \'%s\'%s of type " blue_code (print_binop b) reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt "@." ;
  indent fmt depth ;
  Format.fprintf fmt "-- First expression:@." ;
  pp_texpr fmt (depth + 1) e1 ;
  indent fmt depth ;
  Format.fprintf fmt "-- Second expression:@." ;
  pp_texpr fmt (depth + 1) e2
| TEfunc (id, alist, t) ->
  indent fmt depth ;
  Format.fprintf fmt "%sTEfunc%s named %s\"%s\"%s of type " blue_code reset_code green_code id reset_code;
  pp_typ fmt t ;
  Format.fprintf fmt "@." ;
  indent fmt depth ;
  Format.fprintf fmt "-- List of contained atoms:@." ;
  List.iter (pp_tatom fmt (depth + 1)) alist
| TEif (e1, e2, e3, t) ->
  indent fmt depth ;
  Format.fprintf fmt "%sTEif%s of type " blue_code reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt "@." ;
  indent fmt depth ;
  Format.fprintf fmt "Condition:@." ;
  pp_texpr fmt (depth + 1) e1 ;
  indent fmt depth ;
  Format.fprintf fmt "then:@." ;
  pp_texpr fmt (depth + 1) e2 ;
  indent fmt depth ;
  Format.fprintf fmt "else:@." ;
  pp_texpr fmt (depth + 1) e3
| TEdo (elist, t) ->
  indent fmt depth ;
  Format.fprintf fmt "%sTEdo%s of type " blue_code reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt "@." ;
  indent fmt depth ;
  Format.fprintf fmt "-- List of contained expressions:@." ;
  List.iter (pp_texpr fmt (depth + 1)) elist
| TElet (bind_list, expr, t) ->
  indent fmt depth ;
  Format.fprintf fmt "%sAElet%s of type " blue_code reset_code ;
  pp_typ fmt t ;
  indent fmt depth ;
  Format.fprintf fmt "-- List of bindings:@." ;
  List.iter (fun (name, expr) -> 
      indent fmt (depth + 1) ;
      Format.fprintf fmt "Binding of name %s%s%s with expression:@." green_code name reset_code ;
      pp_texpr fmt (depth + 1) expr
    ) bind_list ;

  indent fmt depth ;
  Format.fprintf fmt "And associated expression:@." ;
  pp_texpr fmt (depth + 1) expr

| TEcase (expr, branch_list, t) ->
  indent fmt depth ;
  Format.fprintf fmt "%sTEcase%s of type " blue_code reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt "@." ;
  indent fmt depth ;
  Format.fprintf fmt "-- List of bindings:@." ;
  List.iteri (fun i (pattern, expr) -> 
      indent fmt (depth + 1) ;
      Format.fprintf fmt "Pattern %d:@." i ;
      pp_pattern fmt (depth+2) pattern;
      indent fmt (depth + 1) ;
      Format.fprintf fmt "Expression %d:@." i ;
      pp_texpr fmt (depth + 2) expr
    ) branch_list ;
  Format.fprintf fmt "-- And associated expression:@." ;
  pp_texpr fmt (depth + 1) expr
(*| TEuident (data_constr, alist, t) -> 
  indent fmt depth ;
  Format.fprintf fmt "%sAEuident%s with data cosntruction %s(%d, %d)%s of type " blue_code reset_code yellow_code (fst data_constr) (snd data_constr) reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt " with offset %s%d%s@." yellow_code i reset_code ;
  indent fmt depth ;
  Format.fprintf fmt "-- List of contained atoms:@." ;
  List.iter (pp_aatom fmt (depth + 1)) alist*)



and pp_patarg fmt depth = function
| Pconst c -> 
  Format.fprintf fmt "%sPconst%s:@." blue_code reset_code ;
  pp_const fmt (depth + 1) c
| Pident id ->
  Format.fprintf fmt "%sAPlident%s %s\"%s\"%s:@." blue_code reset_code green_code id reset_code
| _ -> Format.fprintf fmt "%sCannot print Ppattern yet%s@." red_code reset_code

and pp_tatom fmt depth a = 
indent fmt depth ;
match a with
| TAconst (c, t) ->
  Format.fprintf fmt "%sTAconst%s of type " blue_code reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt "@." ;
  pp_const fmt (depth + 1) c
| TAident (id, t) -> 
  Format.fprintf fmt "%sTAident%s %s\"%s\"%s of type " blue_code reset_code green_code id reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt "@."
| TAexpr (e, t) -> 
  Format.fprintf fmt "%sTAexpr%s of type " blue_code reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt "@." ;
  pp_texpr fmt (depth + 1) e 

and pp_pattern fmt depth p =
  indent fmt depth ;
  match p with 
  | Parg patarg -> 
    Format.fprintf fmt "%sAParg%s with patarg " blue_code reset_code ;
    pp_patarg fmt depth (snd patarg)
  | Pconsarg (id, patarg_list) -> 
    Format.fprintf fmt "%sAPconsarg%s with patarg list:@." blue_code reset_code ;
    List.iter (indent fmt (depth + 1) ; pp_patarg fmt (depth + 1)) (List.map snd patarg_list)
    

let rec pp_aexpr fmt depth = function
| AEatom (a, t, i) -> 
  indent fmt depth ;
  Format.fprintf fmt "%sAEatom%s of type " blue_code reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt " with offset %s%d%s@." yellow_code i reset_code ;
  pp_aatom fmt (depth + 1) a
| AEunop (_, e, t, i) ->
  indent fmt depth ;
  Format.fprintf fmt "%sAEunop%s of type " blue_code reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt " with offset %s%d%s@." yellow_code i reset_code ;
  pp_aexpr fmt (depth + 1) e
| AEbinop (e1, b, e2, t, i) ->
  indent fmt depth ;
  Format.fprintf fmt "%sAEbinop \'%s\'%s of type " blue_code (print_binop b) reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt " with offset %s%d%s@." yellow_code i reset_code ;
  indent fmt depth ;
  Format.fprintf fmt "-- First expression:@." ;
  pp_aexpr fmt (depth + 1) e1 ;
  indent fmt depth ;
  Format.fprintf fmt "-- Second expression:@." ;
  pp_aexpr fmt (depth + 1) e2
| AEfunc (id, alist, t, i) ->
  indent fmt depth ;
  Format.fprintf fmt "%sAEfunc%s named %s\"%s\"%s of type " blue_code reset_code green_code id reset_code;
  pp_typ fmt t ;
  Format.fprintf fmt " with offset %s%d%s@." yellow_code i reset_code ;
  indent fmt depth ;
  Format.fprintf fmt "-- List of contained atoms:@." ;
  List.iter (pp_aatom fmt (depth + 1)) alist
| AEif (e1, e2, e3, t, i) ->
  indent fmt depth ;
  Format.fprintf fmt "%sAEif%s of type " blue_code reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt " with offset %s%d%s@." yellow_code i reset_code ;
  indent fmt depth ;
  Format.fprintf fmt "Condition:@." ;
  pp_aexpr fmt (depth + 1) e1 ;
  indent fmt depth ;
  Format.fprintf fmt "then:@." ;
  pp_aexpr fmt (depth + 1) e2 ;
  indent fmt depth ;
  Format.fprintf fmt "else:@." ;
  pp_aexpr fmt (depth + 1) e3
| AEdo (elist, t, i) ->
  indent fmt depth ;
  Format.fprintf fmt "%sAEdo%s of type " blue_code reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt " with offset %s%d%s@." yellow_code i reset_code ;
  indent fmt depth ;
  Format.fprintf fmt "-- List of contained expressions:@." ;
  List.iter (pp_aexpr fmt (depth + 1)) elist
| AElet (bind_list, expr, t, i) ->
  indent fmt depth ;
  Format.fprintf fmt "%sAElet%s of type " blue_code reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt " with offset %s%d%s@." yellow_code i reset_code ;
  indent fmt depth ;
  Format.fprintf fmt "-- List of bindings:@." ;
  List.iter (fun (uid, expr) -> 
      indent fmt (depth + 1) ;
      Format.fprintf fmt "Binding of uid %s%d%s with expression:@." green_code uid reset_code ;
      pp_aexpr fmt (depth + 1) expr
    ) bind_list ;

  indent fmt depth ;
  Format.fprintf fmt "And associated expression:@." ;
  pp_aexpr fmt (depth + 1) expr

| AEcase (expr, branch_list, t, i) ->
  indent fmt depth ;
  Format.fprintf fmt "%sAEcase%s of type " blue_code reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt " with offset %s%d%s@." yellow_code i reset_code ;
  indent fmt depth ;
  Format.fprintf fmt "-- List of bindings:@." ;
  List.iteri (fun i (pattern, expr) -> 
      indent fmt (depth + 1) ;
      Format.fprintf fmt "Pattern %d:@." i ;
      pp_apattern fmt (depth+2) pattern;
      indent fmt (depth + 1) ;
      Format.fprintf fmt "Expression %d:@." i ;
      pp_aexpr fmt (depth + 2) expr
    ) branch_list ;
  Format.fprintf fmt "-- And associated expression:@." ;
  pp_aexpr fmt (depth + 1) expr
| AEuident (data_constr, alist, t, i) -> 
  indent fmt depth ;
  Format.fprintf fmt "%sAEuident%s with data cosntruction %s(%d, %d)%s of type " blue_code reset_code yellow_code (fst data_constr) (snd data_constr) reset_code ;
  pp_typ fmt t ;
  Format.fprintf fmt " with offset %s%d%s@." yellow_code i reset_code ;
  indent fmt depth ;
  Format.fprintf fmt "-- List of contained atoms:@." ;
  List.iter (pp_aatom fmt (depth + 1)) alist


and pp_apattern fmt depth p =
indent fmt depth ;
match p with 
| AParg patarg -> 
  Format.fprintf fmt "%sAParg%s with patarg " blue_code reset_code ;
  pp_apatarg fmt depth patarg
| APconsarg (id, patarg_list) -> 
  Format.fprintf fmt "%sAPconsarg%s with patarg list:@." blue_code reset_code ;
  List.iter (indent fmt (depth + 1) ; pp_apatarg fmt (depth + 1)) patarg_list
  
and pp_apatarg fmt depth = function
| APconst (c, c_adr, i) -> 
  Format.fprintf fmt "%sAPconst%s with const offset %s%d%s and offset %s%d%s:@." blue_code reset_code yellow_code c_adr reset_code yellow_code i reset_code ;
  pp_const fmt (depth + 1) c
| APlident (id, i) ->
  Format.fprintf fmt "%sAPlident%s %s\"%s\"%s and offset %s%d%s:@." blue_code reset_code green_code id reset_code yellow_code i reset_code
| APuident (uid, i) ->
  Format.fprintf fmt "%sAPuident%s %s%d%s and offset %s%d%s:@." blue_code reset_code yellow_code uid reset_code yellow_code i reset_code
| _ -> Format.fprintf fmt "%sCannot print APpattern yet%s@." red_code reset_code

and pp_aatom fmt depth a = 
indent fmt depth ;
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

let pp_genv fmt genv = 
  Format.fprintf fmt "%sContent of data:@.%s" blue_code reset_code ;
  Smap.iter (fun ident (uid, length) ->
    Format.fprintf fmt "-- Data construction named %s\"%s\"%s with unique id %s%d%s and length %s%d%s.@." green_code ident reset_code yellow_code uid reset_code yellow_code length reset_code
  ) genv ;
  Format.fprintf fmt "@."