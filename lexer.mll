(* Analyseur lexical *)

{
  open Lexing
  open Parser

  (* exception à lever pour signaler une erreur lexicale *)
  exception Lexing_error of string
}

let digit = ['0'-'9']
let lower = ['a'-'z' '_']
let upper = ['A'-'Z']
let other = lower | upper | digit | '\''
let lident = lower other*
let uident = upper (other | '.')*

rule token = parse
  | "--"                  { inline_comment lexbuf }
  | eof                   { EOF }
  | _                     { raise (Lexing_error "construction not supported yet") }

and inline_comment = parse
  | '\n'                  { Lexing.new_line lexbuf ; token lexbuf }
  | eof                   { EOF }
  | _                     { inline_comment lexbuf }