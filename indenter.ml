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

let indent strong buf : token =
  (* TODO: vérifier si la file n'est pas vide, si oui défiler *)
  (* A VERIFIER : est-ce que appeler buf ne consomme pas le lexeme *)
  let c = (lexeme_start_p buf).pos_bol in
  let l = Lexer.token buf in
  (match l with
  | IF | LPAREN | CASE -> fermer c ; Stack.push M pile ; Queue.push l file
  | RPAREN | ELSE | IN -> find_M
  | THEN -> find_M ; Stack.push M pile
  | WHERE | DO -> fermer c ; Queue.push l file ; Queue.push LBRACK file ; failwith "todo"
  | LET -> failwith "todo"
  | OF -> failwith "todo"
  | _ -> fermer c) ;
  Queue.pop file