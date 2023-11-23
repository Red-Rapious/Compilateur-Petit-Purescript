(* Analyseur lexical *)

{
  open Lexing
  open Parser

  (* exception à lever pour signaler une erreur lexicale *)
  exception Lexing_error of string

  let id_or_kwd =
    let h = Hashtbl.create 32 in
    List.iter (fun (s, tok) -> Hashtbl.add h s tok)
      [
        "case", EOF; 
        "class", EOF; 
        "data", EOF;
        "do", EOF; 
        "else", EOF;
        "false", CST (Cbool false); 
        "forall", EOF;
        "if", EOF; 
        "import", EOF; 
        "in", EOF;
        "instance", EOF;
        "let", EOF;
        "module", EOF;
        "of", EOF;
        "then", EOF;
        "true", CST (Cbool true);
        "when", EOF;
      ];
    fun s -> try Hashtbl.find h s with Not_found -> IDENT s
}

let digit = ['0'-'9']
let lower = ['a'-'z' '_']
let upper = ['A'-'Z']
let other = lower | upper | digit | '\''
let lident = lower other*
let uident = upper (other | '.')*
let integer = ('0' | ['1'-'9'] digit*)
let string = "\"" _* "\""

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
  | "="                   { SIMPLE_EQ }
  | integer as i          { CST (Cint (int_of_string i))}
  | "module Main where"   { MODULE_MAIN }
  | "import Prelude\nimport Effect\nimport Effect.Console\n"
                          { new_line lexbuf; new_line lexbuf; new_line lexbuf; IMPORTS }
  | lident as i           { id_or_kwd i }
  | string as s           { CST (Cstring s)}
  | eof                   { EOF }
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