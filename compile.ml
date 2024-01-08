open Format
open X86_64
open Ast

exception UndefIdent of string

module Smap = Map.Make(String)
type local_env = int Smap.t

let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17


(* Décoration de l'AST avec l'allocation des variables *)
(* Retourne un tuple contenant l'AST décoré et la frame size actuelle *)
let rec alloc_decl (decl:tdecl) : adecl =
match decl with
| TDefn d -> alloc_defn d
| TDfdecl d -> alloc_fdecl d
| TDdata d -> alloc_data d
| TDclass c -> alloc_class c
| TDinstance (instance, dlist) -> failwith ""

and alloc_defn (ident, plist, expr) : adecl = 
  let env = ref Smap.empty in 
  let patargs = [] in 
  let a_expr, size = alloc_expr !env 0 expr in 
  ADefn (ident, patargs, a_expr, size)
and alloc_expr (env: local_env) (fpcur: int) = function 
| TEatom a -> alloc_atom env fpcur a
| TEbinop (e1, op, e2, t) -> 
  let e1', fpcur1 = alloc_expr env fpcur e1 in 
  let e2', fpcur2 = alloc_expr env fpcur e2 in
  AEbinop(e1', op, e2', t), max fpcur1 fpcur2
| TEif (e1, e2, e3) ->
  let e1', fpcur1 = alloc_expr env fpcur e1 in 
  let e2', fpcur2 = alloc_expr env fpcur e2 in 
  let e3', fpcur3 = alloc_expr env fpcur e3 in 
  AEif (e1', e2', e3'), max (max fpcur1 fpcur2) fpcur3
| _ -> failwith ""
and alloc_atom (env: local_env) (fpcur: int) = function 
| TAconst (c, t) -> AEatom (alloc_const t fpcur c), fpcur
| TAexpr (e, t) -> alloc_expr env fpcur e (* warning: on drop le type ? quel type choisir entre celui de l'atome et celui de l'expression ? *)
| TAident (ident, t) -> begin
  match ident with
  | "unit" -> AEatom (AAconst (Cbool (true), t)), fpcur
  | _ -> 
    begin
      match Smap.find_opt ident env with
      | Some c -> AEatom (AAident (c, t)), fpcur
      | None -> raise (UndefIdent ident)
    end
  end

and alloc_branch (env: local_env) (fpcur: int) b = failwith ""
and alloc_binding (env: local_env) (fpcur: int) b = failwith ""
and alloc_fdecl fdecl = failwith ""
and alloc_data data = failwith ""
and alloc_class c = failwith ""
and alloc_const t fpcur = function
| Cbool b -> AAconst (Cbool b, t)
| Cstring s -> AAconst (Cstring s, t)
| Cint x -> AAconst (Cint x, t)

let alloc = List.map alloc_decl

(* Production du code *)
let compile_decl (codefun, codemain) d = nop, nop

let compile_program (p : tdecl list) ofile =
  let p = alloc p in
  (*Format.eprintf "%a@." print p;*)
  let codefun, code = List.fold_left compile_decl (nop, nop) p in
  let p =
    { text =
        globl "main" ++ label "main" ++
        movq !%rsp !%rbp ++

        code ++

        (* Exit *)
        movq (imm 0) !%rax ++
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
