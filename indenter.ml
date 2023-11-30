open Parser
open Lexing

type pile_type =
| Block of int
| M

let pile = Stack.create ()
let file = Queue.create ()

let rec fermer c =
  if Stack.length pile = 0 then begin
    Queue.add SEMICOLON file
  end
  else match Stack.top pile with
  | Block n when n > c -> Queue.add LBRACK file ; fermer c
  | _ -> Queue.add SEMICOLON file


let indent strong buf : token =
  let c = (lexeme_start_p buf).pos_bol in
  match Lexer.token buf with
  | IF | LPAREN | CASE -> fermer c ; Stack.push M pile ; Queue.pop file 
  | _ -> failwith "todo"