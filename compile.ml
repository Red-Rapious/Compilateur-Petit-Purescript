open Format
open X86_64
open Ast

exception VarUndef of string

let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

module Smap = Map.Make(String)

type local_env = ident Smap.t

let alloc_decl = function 
| Defn d -> failwith "alloc_decl: todo"
| _ -> failwith "alloc_decl: todo"

let alloc = List.map alloc_decl


let compile_decl (codefun, codemain) = failwith "compile_decl: todo"

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

        label "print_int" ++
        movq !%rdi !%rsi ++
        movq (ilab ".Sprint_int") !%rdi ++
        movq (imm 0) !%rax ++
        call "printf" ++
        ret ++

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
