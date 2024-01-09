open Format
open X86_64
open Ast
open Utility

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
| TDinstance (instance, dlist) -> failwith "alloc_decl: instances todo"

and alloc_defn (ident, plist, expr) : adecl = 
  let env = ref Smap.empty in 
  let fpcur = ref 0 in

  let patargs =
    List.map (fun patarg ->
      let apatarg = alloc_patarg !fpcur patarg in
      fpcur := !fpcur + 8 ;
      (match apatarg with
      | APident (ident, adr) -> env := Smap.add ident adr !env 
      | _ -> ()) ;
      apatarg
    ) plist
  in

  let a_expr, size = alloc_expr !env 0 expr in 
  ADefn (ident, patargs, a_expr, size)

and alloc_expr (env: local_env) (fpcur: int) = function 
| TEatom a -> alloc_atom env fpcur a
| TEbinop (e1, op, e2, t) -> 
  let e1', fpcur1 = alloc_expr env fpcur e1 in 
  let e2', fpcur2 = alloc_expr env fpcur e2 in
  AEbinop(e1', op, e2', t, fpcur), max fpcur1 fpcur2
| TEif (e1, e2, e3, t) ->
  let e1', fpcur1 = alloc_expr env fpcur e1 in 
  let e2', fpcur2 = alloc_expr env fpcur e2 in 
  let e3', fpcur3 = alloc_expr env fpcur e3 in 
  AEif (e1', e2', e3', t, fpcur), max (max fpcur1 fpcur2) fpcur3
| TEfunc (name, args, t) ->
  (* TODO: UPDATE FPCUR *)
  let aargs = List.map (fun x -> 
    (* Très moche *)
    match fst (alloc_atom env fpcur x) with
    | AEatom (a, _, _) -> a 
    | _ -> failwith "une expression devant contenir un atome ne contient pas d'atome"
    ) args in 
  AEfunc (name, aargs, t, fpcur), fpcur
| _ -> failwith "alloc_expr: todo"

and alloc_atom (env: local_env) (fpcur: int) = function 
(* WARNING ATTENTION TODO : augmenter fpcur *)
| TAconst (c, t) -> AEatom (alloc_const t fpcur c, t, fpcur), fpcur
| TAexpr (e, t) -> alloc_expr env fpcur e (* warning: on drop le type ? quel type choisir entre celui de l'atome et celui de l'expression ? *)
| TAident (ident, t) -> begin
  match ident with
  | "unit" -> AEatom (AAconst (Cbool (true), t, fpcur), t, fpcur), fpcur
  | _ -> 
    begin
      match Smap.find_opt ident env with
      | Some c -> AEatom (AAident (t, c), t, fpcur), fpcur
      | None -> raise (UndefIdent ident)
    end
  end

and alloc_patarg fpcur p = 
match p with 
| TPconst c -> APconst (c, fpcur)
| TPident i -> APident (i, fpcur)

and alloc_branch (env: local_env) (fpcur: int) b = failwith "alloc_branch: todo"
and alloc_binding (env: local_env) (fpcur: int) b = failwith "alloc_binding: tood"
and alloc_fdecl fdecl = ADfdecl {
  aname = fdecl.tname ;
  avariables = fdecl.tvariables;
  antypes = fdecl.tntypes;
  atypes = fdecl.ttypes;
  aout_type = fdecl.tout_type
}
and alloc_data data = failwith "alloc_data: todo"
and alloc_class c = failwith "alloc_class: todo"
and alloc_const t fpcur = function
| Cbool b -> AAconst (Cbool b, t, fpcur)
| Cstring s -> AAconst (Cstring s, t, fpcur)
| Cint x -> AAconst (Cint x, t, fpcur)

let alloc = List.map alloc_decl


(* Production du code *)
let round_16 a = match a mod 16 with
| 0 -> a
| i -> a + 16-i

let str_counter = ref 0
let hardcoded_strings = ref []

let rec compile_decl = function 
| ADefn d -> compile_defn d
| ADfdecl d -> nop
| _ -> failwith "compile_decl: todo"

and compile_defn (ident, patargs, expr, size) = 
  label ident ++
  enter (imm (round_16 (abs size))) ++

  compile_expr expr ++
  begin 
    match type_of_aexpr expr with
    | TUnit | TCons ("Effect", [TUnit]) -> movq (imm 0) (reg rax)
    | _ -> failwith "unsupported type of defn to compile"
  end
  ++

  leave ++
  ret

and compile_expr = function 
| AEatom (at, t, ad) -> 
  compile_atom at ++
  movq2idx (address_of_aatom at) rbp ad rbp
| AEfunc (name, params, types, ad) -> 
  (* production du code de calcul des paramètres *)
  List.fold_left (fun code atom -> code ++ compile_atom atom) nop params ++

  (* on stocke les paramètres *)
  (* s'il y a un nombre impair de paramètres, on rajoute un immédiat nul *)
  (if ((List.length params) mod 2) = 1 then pushq (imm 0) else nop) ++
  List.fold_left (fun code atom ->
    pushq (ind ~ofs:(address_of_aatom atom) rbp) ++ code
  ) nop params ++

  (* si la fonction appelée est 'show', on la remplace par la fonction
    codée en assembleur qui correspond au type à afficher *)
  call begin match name with
  | "show" -> begin
      match type_of_aatom (List.hd params) with 
      | TInt -> "print_int"
      | TBool -> "print_bool"
      | _ -> failwith "unsupported show"
    end
  | _ -> name
  end ++

  (* on pop les arguments *)
  List.fold_left (
    fun code atom -> 
    (* TODO *)
    code ++ popq r8
  ) nop params ++

  (* on n'oublie pas de pop une fois de plus si l'on avait ajouté un immédiat factice *)
  if ((List.length params) mod 2) = 1 then popq r8 else nop ++
  movq (reg rax) (ind ~ofs:ad rbp)

| _ -> failwith "compile_expr: todo"

and compile_atom = function 
| AAexpr (expr, t, a) -> 
  let adresse_dest = address_of_aexpr expr in 
  compile_expr expr ++
  movq2idx adresse_dest rbp a rbp
| AAconst (c, t, a) -> 
  let ptr = match c with
  (* si c'est une constante facile, on la représente par un immédiat *)
  | Cint i -> imm i
  | Cbool false -> imm 0
  | Cbool true -> imm 1
  (* sinon, on l'ajoute au segment data, et on renvoie un label *)
  | Cstring s -> 
    let str_label = "hardcoded_string_"^(string_of_int !str_counter) in 
    hardcoded_strings := (str_label, s) :: !hardcoded_strings ;
    incr str_counter ;
    (ilab str_label)
  in
  movq ptr (ind ~ofs:a rbp)
| AAident _ -> nop (* TODO : vérifier que ça marche bien comme ça *)

let compile_program (p : tdecl list) ofile =
  let p = alloc p in
  (*Format.eprintf "%a@." print p;*)
  let code = List.fold_left (fun code tdecl -> code ++ compile_decl tdecl) nop p in
  
  let data = Hashtbl.fold (fun x _ l -> label x ++ dquad [1] ++ l) genv
  begin
    label ".Sprint_int" ++ string "%d\n" ++
    label ".Sprint_string" ++ string "%s\n" ++
    label "true" ++ string "true\n" ++
    label "false" ++ string "false\n" ++
    List.fold_left (fun code (label_name, str) ->
      code ++ label label_name ++ string (str^"\n")
    ) nop !hardcoded_strings
  end
  in

  
  let p =
    { text =
        globl "main" ++ (*label "main" ++
        movq !%rsp !%rbp ++*)

        code ++

        (* Exit *)
        (*movq (imm 0) !%rax ++
        ret ++*)

        (* Afficheur d'entiers *)
        (*movq !%rdi !%rsi ++
        movq (ilab ".Sprint_int") !%rdi ++
        movq (imm 0) !%rax ++*)

        label "print_int" ++
        enter (imm 0) ++
        movq (imm 24) (reg rdi) ++
        call "malloc" ++
        movq (reg rax) (reg rdi) ++
        movq (ilab ".Sprint_int") (reg rsi) ++
        xorq (reg rax) (reg rax) ++
        movq (ind ~ofs:16 rbp) (reg rdx) ++
        call "sprintf" ++
        movq (reg rax) (reg r8) ++
        movq (reg rdi) (reg rax) ++
        subq (reg r8) (reg rax) ++
        addq (reg rax) (reg r8) ++  (* to remove the last character *)
        movb (imm 0) (ind ~ofs:(-1) r8) ++
        leave ++
        ret ++

        label "log" ++
        enter (imm 0) ++
        movq (ind ~ofs:16 rbp) (reg rsi) ++
        movq (ilab ".Sprint_string") (reg rdi) ++
        xorq (reg rax) (reg rax) ++
        call "printf" ++
        leave ++
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
        ret ;

        (*codefun;*)
      data = data
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f
