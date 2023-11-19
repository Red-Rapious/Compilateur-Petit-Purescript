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
let integer = ('0' | ['1'-'9'] digit*)

rule token = parse
  | [' ' '\t' '\r']+      { token lexbuf }
  | "--"                  { inline_comment lexbuf }
  | '\n'                  { new_line lexbuf; token lexbuf }
  | "{-"                  { comment lexbuf }
  | '+'                   { PLUS }
  | '-'                   { MINUS }
  | '*'                   { TIMES }
  | "/"                   { DIV }
  | "=="                  { CMP Beq }
  | "!="                  { CMP Bneq }
  | "<"                   { CMP Blt }
  | "<="                  { CMP Ble }
  | ">"                   { CMP Bgt }
  | ">="                  { CMP Bge }
  | eof                   { EOF }
  | "module Main where"   { MODULE_MAIN }
  | "import Prelude\nimport Effect\nimport Effect.Console\n"
                          { new_line lexbuf; new_line lexbuf; new_line lexbuf; IMPORTS }
  | _ as c                { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

and inline_comment = parse
  | '\n'              { Lexing.new_line lexbuf ; token lexbuf }
  | eof               { EOF }
  | _                 { inline_comment lexbuf }

and comment = parse
  | "-}"    { token lexbuf }
  | '\n'    { new_line lexbuf; comment lexbuf }
  | _       { comment lexbuf }
  | eof     { raise (Lexing_error ("unterminated comment")) }