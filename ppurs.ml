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
  (*let f = open_in !ifile in*)

  (* Création d'un tampon d'analyse lexicale *)
  (*let buf = Lexing.from_channel f in ()*)