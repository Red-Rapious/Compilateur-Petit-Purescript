open Parser
open Lexing

exception IndentationError of string

type pile_type =
| Block of int
| M

let pile = Stack.create ()
let file = Queue.create ()

let rec fermer c =
  if Stack.length pile = 0 then Queue.add SEMICOLON file
  else match Stack.top pile with
  | Block n when n > c -> Queue.add LBRACK file ; fermer c
  | _ -> Queue.add SEMICOLON file

(*let rec find_M = 
  if Stack.length pile = 0 then failwith "Erreur : pile vide, impossible de trouver M"
  else begin 
    match Stack.pop pile with 
    | M -> ()
    | Block _ -> Queue.add RBRACK file ; find_M
  end*)

let find_M = 
  let depile = ref true in 
  while !depile do 
    if Stack.length pile = 0 then raise (IndentationError "tentative de dépiler une pile vide lors de la recherche de M")
    else begin 
      match Stack.pop pile with 
      | M -> depile := false
      | Block _ -> Queue.add RBRACK file
    end
  done

let rec traiter_lexeme strong buf =
  (* A VERIFIER : est-ce que appeler buf ne consomme pas le lexeme *)
  let c = (lexeme_start_p buf).pos_bol in
  let l = Lexer.token buf in
  match l with
  | IF | LPAREN | CASE -> if strong then fermer c ; Stack.push M pile ; Queue.push l file
  | RPAREN | ELSE | IN -> find_M
  | THEN -> find_M ; Stack.push M pile
  | WHERE | DO | LET | OF -> 
    fermer c ; 
    if l = LET then Stack.push M pile ;
    if l = OF then find_M ;
    Queue.push l file ; 
    Queue.push LBRACK file ; 
    let c' = (lexeme_start_p buf).pos_bol in
    if strong then fermer c' ;
    Stack.push (Block c') pile ;
    traiter_lexeme false buf
  | _ -> fermer c

let rec indent strong buf : token =
  if Queue.is_empty file then traiter_lexeme strong buf ;
  Queue.pop file