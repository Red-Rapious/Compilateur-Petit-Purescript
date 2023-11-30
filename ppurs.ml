open Format
open Lexing

(* Option de compilation, pour s'arrêter à l'issue du parser *)
let parse_only = ref false
let type_only = ref false

(* Nom du fichier source *)
let ifile = ref ""

let set_file f s = f := s

(* Les options du compilateur que l'on affiche avec --help *)
let options =
  ["--parse-only", Arg.Set parse_only, "  Réaliser uniquement la phase d'analyse syntaxique";
  "--type-only", Arg.Set type_only, " Réaliser uniquement l'analyse syntaxique et sémantique"
  ]

let usage = "usage: ppurs [options] file.purs"

(* localise une erreur en indiquant la ligne et la colonne *)
let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

let () =
  (* Parsing de la ligne de commande *)
  Arg.parse options (set_file ifile) usage;

  (* On vérifie que le nom du fichier source a bien été indiqué *)
  if !ifile="" then begin eprintf "Aucun fichier à compiler\n@?"; exit 1 end;

  (* Ce fichier doit avoir l'extension .purs *)
  if not (Filename.check_suffix !ifile ".purs") then begin
    eprintf "Le fichier d'entrée doit avoir l'extension .purs\n@?";
    Arg.usage options usage;
    exit 1
  end;

  (* Ouverture du fichier source en lecture *)
  let f = open_in !ifile in

  (* Création d'un tampon d'analyse lexicale *)
  let buf = Lexing.from_channel f in
  try
    (* Parsing: la fonction  Parser.prog transforme le tampon lexical en un
       arbre de syntaxe abstraite si aucune erreur (lexicale ou syntaxique)
       n'est détectée.
       La fonction Lexer.token est utilisée par Parser.prog pour obtenir
       le prochain token. *)
    let _p = Parser.file (Indenter.indent false) buf  in
    close_in f;

    (* On s'arrête ici si on ne veut faire que le parsing *)
    if !parse_only then exit 0;
    failwith "Erreur: seul le parsing est implémenté pour l'instant"
    (*Interp.prog p*)
  with
    (* Erreur lexicale. On récupère sa position absolue et on la convertit en numéro de ligne *)
  | Lexer.Lexing_error c ->
    localisation (Lexing.lexeme_start_p buf);
    eprintf "Erreur lexicale: %s@." c;
    exit 1
    
    (* Erreur syntaxique. On récupère sa position absolue et on la convertit en numéro de ligne *)
    | Parser.Error ->
    localisation (Lexing.lexeme_start_p buf);
    eprintf "Erreur syntaxique@.";
    exit 1
      
    (* Erreur pendant l'interprétation *)
    (*| Interp.Error s->
    eprintf "Erreur : %s@." s;
    exit 1*)
