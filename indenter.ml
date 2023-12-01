open Parser
open Lexing

exception IndentationError of string

type pile_type =
| Block of int
| M

let pile = Stack.create ()
let file = Queue.create ()

let rec fermer c =
  if Stack.length pile <> 0 then 
  match Stack.top pile with
  | Block n when n > c -> 
    let _ = Stack.pop pile in 
    Queue.add RBRACK file ; 
    fermer c
  | Block n when n = c -> Queue.add SEMICOLON file
  | _ -> ()

let find_M () = 
  let depile = ref true in 
  while !depile do 
    if Stack.length pile = 0 then raise (IndentationError "tentative de dépiler une pile vide lors de la recherche de M")
    else begin 
      match Stack.pop pile with 
      | M -> depile := false
      | Block _ -> Queue.add RBRACK file
    end
  done

let rec traiter_lexeme strong l buf =
  let pos = lexeme_start_p buf in
  let c = pos.pos_cnum - pos.pos_bol in 
  match l with
  | IF | LPAREN | CASE -> 
    if strong then fermer c ; 
    Stack.push M pile ; 
    Queue.push l file
  | RPAREN | THEN | ELSE | IN -> 
    find_M () ; 
    if l = THEN then Stack.push M pile ; 
    Queue.push l file
  | WHERE | DO | LET | OF -> 
    if strong then fermer c ; 
    if l = LET then Stack.push M pile ;
    if l = OF then find_M () ;
    Queue.push l file ; 
    Queue.push LBRACK file ; 
    let t = Lexer.token buf in
    let pos' = lexeme_start_p buf in
    let c' = pos'.pos_cnum - pos'.pos_bol in
    if strong then fermer c' ;
    Stack.push (Block c') pile ;
    traiter_lexeme false t buf
  | EOF -> if strong then fermer (-1) ; Queue.push l file
  | _ -> if strong then fermer c ; Queue.push l file

let rec indent strong buf : token =
  if Queue.is_empty file then traiter_lexeme strong (Lexer.token buf) buf ;
  let t = Queue.pop file in 
  Format.printf "%s" (Pretty.print_token t) ;
  t