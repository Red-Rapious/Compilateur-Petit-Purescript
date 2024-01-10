open Format
open X86_64
open Ast
open Utility

exception UndefIdent of string

module Smap = Map.Make(String)
type local_env = int Smap.t

let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17
type tfpcur = (unit -> int)

let create_neg_fpcur () = 
  let fpcur = ref 0 in 
  (fun () -> fpcur := !fpcur - 8 ; !fpcur)

let create_pos_fpcur () =
  let fpcur = ref 8 in 
  (fun () -> fpcur := !fpcur + 8 ; !fpcur)

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
  let fpcur = create_pos_fpcur () in
  let env = ref Smap.empty in 

  let patargs =
    List.map (fun patarg ->
      let apatarg = alloc_patarg fpcur patarg in
      (match apatarg with
      | APident (ident, adr) -> env := Smap.add ident adr !env 
      | _ -> ()) ;
      apatarg
    ) plist
  in

  let fpcur' = create_neg_fpcur () in
  let a_expr = alloc_expr !env fpcur' expr in 
  ADefn (ident, patargs, a_expr, abs (fpcur' ()))

and alloc_expr (env: local_env) (fpcur: tfpcur) : (texpr -> aexpr)= function 
| TEatom (a, t) -> 
  let aatom = alloc_atom env fpcur a
  in AEatom(aatom, t, fpcur ())
| TEbinop (e1, op, e2, t) -> 
  let e1' = alloc_expr env fpcur e1 in 
  let e2' = alloc_expr env fpcur e2 in
  AEbinop(e1', op, e2', t, fpcur ())
| TEif (e1, e2, e3, t) ->
  let e1' = alloc_expr env fpcur e1 in 
  let e2' = alloc_expr env fpcur e2 in 
  let e3' = alloc_expr env fpcur e3 in 
  AEif (e1', e2', e3', t, fpcur ())
| TEfunc (name, targs, t) ->
  let aargs = List.map (alloc_atom env fpcur) targs in 
  AEfunc (name, aargs, t, fpcur ())
| TEdo (expr_list, t) -> 
  let aexpr_list = List.map (alloc_expr env fpcur) expr_list in
  AEdo (aexpr_list, t, fpcur ())
| _ -> failwith "alloc_expr: todo"

and alloc_atom (env: local_env) (fpcur: tfpcur) : (tatom -> aatom) = function 
| TAconst (c, t) -> AAconst (c, t, fpcur ())
| TAexpr (e, t) -> 
  let aexpr = alloc_expr env fpcur e in 
  AAexpr (aexpr, t, address_of_aexpr aexpr)
| TAident (ident, t) -> begin
  match ident with
  | "unit" -> AAconst (Cbool (false), t, fpcur ())
  | _ -> 
    begin
      match Smap.find_opt ident env with
      | Some c -> AAident (t, c)
      | None -> raise (UndefIdent ident)
    end
  end

and alloc_patarg fpcur p = 
match p with 
| TPconst c -> APconst (c, fpcur ())
| TPident i -> APident (i, fpcur ())

and alloc_branch (env: local_env) (fpcur: tfpcur) b = failwith "alloc_branch: todo"
and alloc_binding (env: local_env) (fpcur: tfpcur) b = failwith "alloc_binding: tood"
and alloc_fdecl fdecl = ADfdecl {
  aname = fdecl.tname ;
  avariables = fdecl.tvariables;
  antypes = fdecl.tntypes;
  atypes = fdecl.ttypes;
  aout_type = fdecl.tout_type
}
and alloc_data data = failwith "alloc_data: todo"
and alloc_class c = failwith "alloc_class: todo"

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

and compile_defn (ident, patargs, expr, fpmax) = 
  label ident ++
  enter (imm (round_16 (abs fpmax))) ++

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
| AEatom (at, t, res_adr) -> 
  compile_atom at ++
  movq2idx (address_of_aatom at) rbp res_adr rbp
| AEfunc (name, params, types, res_adr) -> 
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
  (if ((List.length params) mod 2) = 1 then popq r8 else nop) ++
  movq (reg rax) (ind ~ofs:res_adr rbp)
| AEbinop (e1, binop, e2, t, a) -> 
  begin 
    match binop with
    | Badd | Bsub | Bmul | Bdiv | Bor | Band -> compile_binop (e1, binop, e2, t, a)
    | _ -> compile_binop_compare (e1, binop, e2, t, a)
  end
| AEdo (expr_list, t, res_adr) ->
  List.fold_left (fun code expr -> code ++ compile_expr expr) nop expr_list ++
  movq (imm 0) (ind ~ofs:res_adr rbp)
| _ -> failwith "compile_expr: todo"

and compile_binop (e1, binop, e2, t, res_adr) =
  compile_expr e1 ++
  compile_expr e2 ++
  let a1, a2 = address_of_aexpr e1, address_of_aexpr e2 in 
  (* le cas de la division est un peu spécial *)
  if binop = Bdiv then begin
    (*movq (ind ~ofs:a1 rbp) (reg rax) ++
    movq (ind ~ofs:a2 rbp) (reg rbx) ++
    cqto ++ 
    idivq !%rbx ++
    movq (reg rax) (ind ~ofs:a rbp)*)
    pushq (ind ~ofs:a2 rbp) ++
    pushq (ind ~ofs:a1 rbp) ++
    call "_div" ++
    movq (reg rax) (ind ~ofs:res_adr rbp)
  end
  else begin
    (* on choisit l'instruction assembleur correspondant au calcul *)
    let instruction = match binop with 
    | Badd -> addq
    | Bsub -> subq 
    | Bmul -> imulq
    | Bor -> orq
    | Band -> andq
    | _ -> failwith "ce cas n'est pas sensé se produire"
    in 
    movq (ind ~ofs:a1 rbp) (reg r8) ++
    instruction (ind ~ofs:a2 rbp) (reg r8) ++
    movq (reg r8) (ind ~ofs:res_adr rbp)
  end

and compile_binop_compare (e1, binop, e2, t, a) =
  failwith "compile_binop_compare: todo"

and compile_atom = function 
| AAexpr (expr, t, res_adr) -> 
  let adresse_dest = address_of_aexpr expr in 
  compile_expr expr ++
  movq2idx adresse_dest rbp res_adr rbp
| AAconst (c, t, res_adr) -> 
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
  movq ptr (ind ~ofs:res_adr rbp)
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
        globl "main" ++
        code ++

        (* afficheur d'entiers *)
        (* TODO : modifier *)
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
        addq (reg rax) (reg r8) ++  (* pour retirer le dernier charactère *)
        movb (imm 0) (ind ~ofs:(-1) r8) ++
        leave ++
        ret ++

        (* fonction log de purescript *)
        label "log" ++
        enter (imm 0) ++
        movq (ind ~ofs:16 rbp) (reg rsi) ++
        movq (ilab ".Sprint_string") (reg rdi) ++
        xorq (reg rax) (reg rax) ++
        call "printf" ++
        leave ++
        ret ++

        (* fonction de division *)
        (* la complexité vient du fait que idivq peut retourner 
          un résultat négatif, ce que l'on ne veut pas (cf. arith2.out) *)
        label "_div" ++
        enter (imm 0) ++
        movq (ind ~ofs:16 rbp) (reg rax) ++
        (* on place les drapeaux pour tester le signe de rax *)
        testq (reg rax) (reg rax) ++
        (* si négatif, on appelle la fonction dédiée *)
        js "_div_s" ++
        movq (ind ~ofs:24 rbp) (reg rcx) ++
        movq (imm 0) (reg rdx) ++
        idivq (reg rcx) ++
        leave ++
        ret ++

        label "_div_s" ++
        movq (ind ~ofs:24 rbp) (reg rcx) ++
        movq (imm (-1)) (reg rdx) ++
        idivq (reg rcx) ++
        (* on teste si rdx est non-nul *)
        testq (reg rdx) (reg rdx) ++
        jnz "_div_1" ++
        leave ++
        ret ++

        (* si rdx est non-nul *)
        label "_div_1" ++
        (* si le quotient est positif, on retire 1 *)
        cmpq (imm 0) (ind ~ofs:24 rbp) ++
        jg "_decq_leave" ++
        incq (reg rax) ++
        leave ++
        ret ++

        label "_decq_leave" ++
        decq (reg rax) ++
        leave ++
        ret ++

        (* fonction mod de purescript *)
        (* là encore, la complexité vient du fait que idivq peut retourner 
          un résultat négatif, ce que l'on ne veut pas (cf. arith2.out) *)
        (* TODO: traiter les cas négatifs *)
        label "mod" ++
        enter (imm 0) ++
        movq (ind ~ofs:16 rbp) (reg rax) ++
        movq (ind ~ofs:24 rbp) (reg rbx) ++
        cqto ++ 
        idivq !%rbx ++
        (* on met le résultat dans rax, qui sera ensuite 
           recopié sur la pile par l'instruction suivante (cf. compile_expr)*)
        movq (reg rdx) (reg rax) ++
        leave ++
        ret ++

        (* afficheur de booléen *)
        label "print_bool" ++
        enter (imm 0) ++
        cmpq (imm 0) (ind ~ofs:16 rbp) ++
        je ".Lfalse" ++
        movq (ilab "true") (reg rax) ++
        leave ++
        ret ++
        
        label ".Lfalse" ++
        movq (ilab "false") (reg rax) ++
        leave ++
        ret ++

        (* la fonction pure de purescript *)
        label "pure" ++
        enter (imm 0) ++
        (*movq (imm 0) (reg rax) ++*)
        leave ++
        ret

        (*label ".Lprint" ++
        movq (imm 0) !%rax ++
        call "printf" ++
        leave ++
        ret*)
      ;
      data = data
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f
