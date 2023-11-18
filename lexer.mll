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
  | "--" [^ '\n']* '\n'
  | '\n'                  { new_line lexbuf; token lexbuf }
  | eof                   { EOF }
  | "{-"                  { comment lexbuf }
  | _                     { raise (Lexing_error "construction not supported yet") }

and comment = parse
  | "-}"    { token lexbuf }
  | '\n'    { new_line lexbuf; comment lexbuf }
  | _       { comment lexbuf }
  | eof     { raise (Lexing_error ("unterminated comment")) }