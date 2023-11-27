(* Analyseur lexical *)

{
  open Lexing
  open Parser

  (* exception à lever pour signaler une erreur lexicale *)
  exception Lexing_error of string

  let string_buffer = Buffer.create 1024

  let id_or_kwd =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, tok) -> Hashtbl.add h s tok)
      [
        "case", CASE; 
        "class", CLASS; 
        "data", DATA;
        "do", DO; 
        "else", ELSE;
        "false", CST (Cbool false); 
        "forall", FORALL;
        "if", IF; 
        (*"import", IMPORT;*)
        "in", IN;
        "instance", INSTANCE;
        "let", LET;
        (*"module", MODULE;*)
        "of", OF;
        "then", THEN;
        "true", CST (Cbool true);
        "where", WHERE;
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
  | "<>"                  { CONCAT }
  | "("                   { LPAREN }
  | ")"                   { RPAREN }
  | "->"                  { SIMPLE_ARROW }
  | "=>"                  { DOUBLE_ARROW }
  | "::"                  { DOUBLE_POINTS }
  | ";"                   { SEMICOLON }
  | "|"                   { VBAR }
  | integer as i          { CST (Cint (int_of_string i))}
  | "module Main where"   { MODULE_MAIN }
  | "import Prelude\nimport Effect\nimport Effect.Console\n"
                          { new_line lexbuf; new_line lexbuf; new_line lexbuf; IMPORTS }
  | lident as i           { id_or_kwd i }
  | '"'                   { CST (Cstring (string lexbuf)) }
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

and string = parse
  | '"'     { 
    let s = Buffer.contents string_buffer in
	  Buffer.reset string_buffer;
	  s }
  | "\\n"   { 
    Buffer.add_char string_buffer '\n';
	  string lexbuf }
  | "\\\""  { 
    Buffer.add_char string_buffer '"';
	  string lexbuf }
  | "\\" [' ' '\t' '\r']+ "\\"  { string lexbuf }
  | _ as c  { 
    Buffer.add_char string_buffer c;
	  string lexbuf }
  | eof     { raise (Lexing_error "unterminated string") }