open Format
open Lexing
open Typing

(* Option de compilation, pour s'arrêter à l'issue du parser *)
let parse_only = ref false
let type_only = ref false

(* Nom des fichiers input et output *)
let ifile = ref ""
let ofile = ref ""

let set_file f s = f := s

(* Les options du compilateur que l'on affiche avec --help *)
let options =
  ["--parse-only", Arg.Set parse_only, "  Réaliser uniquement la phase d'analyse syntaxique";
  "--type-only", Arg.Set type_only, " Réaliser uniquement l'analyse syntaxique et sémantique";
  "-o", Arg.String (set_file ofile), "<file>  Pour indiquer le nom du fichier de sortie"
  ]

let usage = "usage: ppurs [options] file.purs"

(* Localise une erreur à une seule position en indiquant la ligne et la colonne *)
let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

(* Localise une erreur à deux positions en indiquant la/les lignes et les caractères concernés*)
let double_localisation (pstart, pend) = 
  let l1 = pstart.pos_lnum 
  and l2 = pend.pos_lnum in
  let cstart = pstart.pos_cnum - pend.pos_bol 
  and cend = pend.pos_cnum - pend.pos_bol in
  if l1 = l2 then begin
    eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l1 cstart cend ;

    (* On ouvre le fichier et on lit la ligne correspondante *)
    let file = open_in !ifile in 
    for _=0 to l1-2 do 
      ignore (input_line file)
    done ;
    (* on affiche la ligne avec son numéro *)
    eprintf "%d | %s\n" l1 (input_line file) ;
    (* on souligne la localisation de l'erreur *)
    eprintf "%s%s\n" (String.make (cstart+3+(String.length (string_of_int l1))) ' ') (String.make (cend-cstart) '^') ;
  end
  else 
    eprintf "File \"%s\", line %d-%d, characters %d-%d:\n" !ifile l1 l2 cstart cend

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

  (* Par défaut, le fichier cible a le même nom que le fichier source, 
  seule l'extension change *)
  if !ofile="" then ofile := Filename.chop_suffix !ifile ".exp" ^ "s";

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
    let program = Parser.file (Indenter.indent true) buf  in
    close_in f;

    (* On s'arrête ici si on ne veut faire que le parsing *)
    if !parse_only then exit 0;
    
    let typed_file = typ_file program in
    if typed_file.tmodule_name <> "Main" then
      eprintf "Warning: le nom du module n'est pas 'Main'@." ;
    if !type_only then exit 0;

    Compile.compile_program typed_file.tmain !ofile
  with
    (* Erreur lexicale *)
    | Lexer.Lexing_error c ->
      localisation (Lexing.lexeme_start_p buf);
      eprintf "Erreur lexicale : %s@." c;
      exit 1

    (* Erreur d'indentation *)
    | Indenter.IndentationError c -> 
      localisation (Lexing.lexeme_start_p buf);
      eprintf "Erreur d'indentation : %s@." c;
      exit 1
    
    (* Erreur syntaxique *)
    | Parser.Error ->
      localisation (Lexing.lexeme_start_p buf);
      eprintf "Erreur syntaxique@.";
      exit 1

    (* Erreur classique de typage *)
    | TypingError (l, s) ->
      double_localisation l ;
      eprintf "Erreur de typage : %s@." s;
      exit 1

    (* Pattern matching vide *)
    | EmptyPatternMatching l ->
      double_localisation l ;
      eprintf "Pattern matching vide.@." ;
      exit 1

    (* Identifiant inconnu *)
    | UnknownIdent (l, id) ->
      double_localisation l ;
      eprintf "Identifiant inconnu : '%s'@." id ;
      exit 1

    | NonExhaustivePatternMatching loc ->
      double_localisation loc ;
      eprintf "Ce pattern matching n'est pas exhaustif.@." ;
      exit 1

    | TooManyArguments loc ->
      double_localisation loc ;
      eprintf "Trop d'arguments on été passés à la fonction.@." ;
      exit 1

    | IdentUsedTwice (loc, id) ->
      double_localisation loc ;
      eprintf "L'identifiant '%s' est utilisé plusieurs fois.@." id ;
      exit 1

    | MultipleFiltering loc ->
      double_localisation loc ;
      eprintf "Le filtrage multiple n'est pas supporté.@." ;
      exit 1

    | SimilarNames (loc, obj_name) ->
      double_localisation loc ;
      eprintf "Des %s avec des noms identiques sont utilisés.@." obj_name ;
      exit 1

    | EmptyDefinition loc ->
      double_localisation loc ;
      eprintf "Définition vide.@." ;
      exit 1

    | DoubleDefinition (loc, id) ->
      double_localisation loc ;
      eprintf "Définition double de '%s'.@." id;
      exit 1

    | AlreadyDefined (loc, obj_name, id) ->
      double_localisation loc ;
      eprintf "%s avec le nom '%s' a déjà été défini.@." obj_name id ;
      exit 1

    | UnifiableInstances loc ->
      double_localisation loc ;
      eprintf "Les instances peuvent être unifiées.@." ;
      exit 1

    | MissingDefinition loc ->
      double_localisation loc ;
      eprintf "Définition manquante.@." ;
      exit 1

    | MissingMain ->
      localisation (Lexing.dummy_pos) ;
      eprintf "Le programme de contient pas de définition de 'main'.@." ;
      exit 1

    | MultipleFilteredVariables loc ->
      double_localisation loc ;
      eprintf "Impossible de filtrer sur des variables multiples.@." ;
      exit 1

    | BadTypesNumber loc ->
      double_localisation loc ;
      eprintf "Le nombre de type ne correspond pas au nombre attendu.@." ;
      exit 1

    (* Erreur de OCaml (ou failwith) *)
    | e ->
      eprintf "Erreur interne du compilateur : %s\n@." (Printexc.to_string e);
      exit 2