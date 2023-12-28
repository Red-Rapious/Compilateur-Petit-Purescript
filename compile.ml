open Format
open X86_64
open Ast

exception VarUndef of string

module Smap = Map.Make(String)
type local_env = ident Smap.t

let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17


(* Décoration de l'AST avec l'allocation des variables *)
(* Retourne un tuple contenant l'AST décoré et la frame size actuelle *)
let rec alloc_decl = function 
| Defn d -> alloc_defn d
| Dfdecl d -> alloc_fdecl d
| Ddata d -> alloc_data d
| Dclass c -> alloc_class c
| Dinstance (instance, dlist) -> List.iter alloc_defn dlist

and alloc_defn (i, l, e) = ()
and alloc_expr (env: local_env) (fpcur: int) = function 
| Eatom a -> alloc_atom env fpcur (snd a)
| _ -> ()
and alloc_atom (env: local_env) (fpcur: int) = function 
| Aconst c -> ()
| _ -> ()
and alloc_branch (env: local_env) (fpcur: int) b = ()
and alloc_binding (env: local_env) (fpcur: int) b = ()
and alloc_fdecl fdecl = ()
and alloc_data data = ()
and alloc_class c = ()

let alloc = List.map alloc_decl

(* Production du code *)
let compile_decl (codefun, codemain) d = nop, nop

let compile_program p ofile =
  let p = alloc p in
  (*Format.eprintf "%a@." print p;*)
  let codefun, code = List.fold_left compile_decl (nop, nop) p in
  let p =
    { text =
        globl "main" ++ label "main" ++
        movq !%rsp !%rbp ++

        code ++

        movq (imm 0) !%rax ++ (* exit *)
        ret ++

        (* Afficheur d'entiers *)
        label "print_int" ++
        movq !%rdi !%rsi ++
        movq (ilab ".Sprint_int") !%rdi ++
        movq (imm 0) !%rax ++
        call "printf" ++
        ret ++

        (* On ajoute un afficheur de booléens au code produit,
           même si on a pas besoin d'afficher de booléens *)
        label "print_bool" ++
        cmpq (imm 0) !%rdi ++
        je ".Lfalse" ++
        movq (ilab "true") !%rdi ++
        jmp ".Lprint" ++

        label ".Lfalse" ++
        movq (ilab "false") !%rdi ++

        label ".Lprint" ++
        movq (imm 0) !%rax ++
        call "printf" ++
        ret ++

        codefun;
      data =
        Hashtbl.fold (fun x _ l -> label x ++ dquad [1] ++ l) genv
          begin
            label ".Sprint_int" ++ string "%d\n" ++
            label "true" ++ string "true\n" ++
            label "false" ++ string "false\n"
          end
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f
