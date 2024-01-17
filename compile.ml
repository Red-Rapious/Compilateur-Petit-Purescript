open Format
open X86_64
open Ast
open Utility

exception UndefIdent of string
let dbg = ref false

(* ALLOCATION DES VARIABLES *)
module Smap = Map.Make(String)
type local_env = int Smap.t

type tfpcur = (unit -> int)

let create_neg_fpcur () = 
  let fpcur = ref 0 in 
  (fun () -> fpcur := !fpcur - 8 ; !fpcur)

let create_pos_fpcur () =
  let fpcur = ref 8 in 
  (fun () -> fpcur := !fpcur + 8 ; !fpcur)

(* Retourne un tuple contenant l'AST décoré et la frame size actuelle *)
let rec alloc_decl genv (decl:tdecl) : adecl =
match decl with
| TDefn d -> alloc_defn genv d
| TDfdecl d -> alloc_fdecl d
| TDdata d -> alloc_data d
| TDclass c -> alloc_class c
| TDinstance (instance, dlist) -> failwith "alloc_decl: instances todo"

and alloc_defn genv (ident, plist, expr) : adecl = 
  let fpcur = create_pos_fpcur () in
  let env = ref Smap.empty in 

  let patargs =
    List.map (fun patarg ->
      let apatarg, env' = alloc_patarg genv fpcur patarg in
      env := Smap.union (
        fun ident -> raise (Typing.IdentUsedTwice (Typing.placeholder_loc, ident))
      ) !env env' ;
      apatarg
    ) plist
  in

  let fpcur' = create_neg_fpcur () in
  let a_expr = alloc_expr genv !env fpcur' expr in 
  ADefn (ident, patargs, a_expr, abs (fpcur' ()))

and alloc_expr genv (env: local_env) (fpcur: tfpcur) : (texpr -> aexpr)= function 
| TEatom (a, t) -> 
  let aatom = alloc_atom genv env fpcur a
  in AEatom(aatom, t, fpcur ())
| TEbinop (e1, op, e2, t) -> 
  let e1' = alloc_expr genv env fpcur e1 in 
  let e2' = alloc_expr genv env fpcur e2 in
  begin
    match op with 
    | Bneq -> 
      let negation = AEbinop(e1', Beq, e2', t, fpcur ()) in
      AEfunc ("not", [AAexpr (negation, type_of_aexpr negation, address_of_aexpr negation)], t, fpcur ())
    | _ -> AEbinop(e1', op, e2', t, fpcur ())
  end
| TEunop (op, e, t) ->
  let e' = alloc_expr genv env fpcur e in 
  AEunop (op, e', t, fpcur ())
| TEif (e1, e2, e3, t) ->
  let e1' = alloc_expr genv env fpcur e1 in 
  let e2' = alloc_expr genv env fpcur e2 in 
  let e3' = alloc_expr genv env fpcur e3 in 
  AEif (e1', e2', e3', t, fpcur ())
| TEfunc (name, targs, t) ->
  let aargs = List.map (alloc_atom genv env fpcur) targs in 
  AEfunc (name, aargs, t, fpcur ())
| TEdo (expr_list, t) -> 
  let aexpr_list = List.map (alloc_expr genv env fpcur) expr_list in
  AEdo (aexpr_list, t, fpcur ())
| TElet (binding_list, expr, typ) ->
  (* TODO: faire les deux boucles en une seule *)
  (* on ajoute à l'environnement les positions des bindings *)
  let env = List.fold_left (
    fun acc_env (bident, _btype) -> 
      Smap.add bident (fpcur ()) acc_env
    ) env binding_list 
  in

  (* allocation des expressions dans le binding *)
  let abinding = List.fold_left (fun l (bident, bexpr) ->
    begin 
      let expr = alloc_expr genv env fpcur bexpr in
      let res_adr = Smap.find bident env in
      (res_adr, expr)
    end :: l
  ) [] binding_list 
  in

  (* allocation de l'expression dans le in *)
  let aexpr = alloc_expr genv env fpcur expr in
  AElet (abinding, aexpr, typ, fpcur ())

| TEcase (texpr, tblist, t) -> 
  let aexpr = alloc_expr genv env fpcur texpr 
  and ablist = List.map (alloc_branch genv env fpcur) tblist in 
  AEcase (aexpr, ablist, t, fpcur ())

and alloc_atom genv (env: local_env) (fpcur: tfpcur) : (tatom -> aatom) = function 
| TAconst (c, t) -> let c_adr = fpcur () in AAconst (c, c_adr, t, c_adr)
| TAexpr (e, t) -> 
  let aexpr = alloc_expr genv env fpcur e in 
  AAexpr (aexpr, t, address_of_aexpr aexpr)
| TAident (ident, t) -> 
  (* TODO : séparer TAuident et TAlident *)
  let first_letter = String.get ident 0 in
  if first_letter = Char.lowercase_ascii first_letter then
  (match ident with
  | "unit" -> AAconst (Cbool (false), fpcur (), t, fpcur ())
  | _ -> 
    begin
      match Smap.find_opt ident env with
      | Some c -> AAlident (t, c)
      | None -> begin
        match Smap.find_opt ident genv with
        | Some c -> 
          let address = fpcur () in 
          AAlident (t, address)
        | None -> raise (UndefIdent ident)
      end
    end)
  else begin
    let address = fpcur () in 
    let data_constr = Smap.find ident genv
    in 
    if snd data_constr <> 0 then failwith "wesh ya des paramètres" else
    AAuident (fst data_constr, t, address)
  end

and alloc_patarg genv fpcur = function
| Pconst c -> APconst (c, fpcur ()), Smap.empty
| Pident ident -> 
  let first_letter = String.get ident 0 in
  if first_letter = Char.lowercase_ascii first_letter then begin
    let address = fpcur ()
    and env = Smap.empty in
    APlident (ident, address), Smap.add ident address env
  end else 
    APuident (fst (Smap.find ident genv), fpcur ()), Smap.empty
| _ -> failwith "alloc_patarg: todo"

and alloc_branch genv (env: local_env) (fpcur: tfpcur) (tpattern, texpr) = 
  let apattern, env' = alloc_pattern genv fpcur tpattern in 
  let union_env = Smap.union (fun id address1 _ -> 
    if id = "_" then Some address1 
    else failwith "cette union est sensé réussir"
  ) env env' in
  (apattern, alloc_expr genv union_env fpcur texpr)

and alloc_pattern genv fpcur = function 
| Parg (l, tpatarg) -> 
  (* TODO : se débarasser de la localisation *)
  let apatarg, env = alloc_patarg genv fpcur tpatarg in 
  AParg apatarg, env
| _ -> failwith "alloc_pattern: todo"
and alloc_binding (env: local_env) (fpcur: tfpcur) b = failwith "alloc_binding: todo"
and alloc_fdecl fdecl = ADfdecl {
  aname = fdecl.tname ;
  avariables = fdecl.tvariables;
  antypes = fdecl.tntypes;
  atypes = fdecl.ttypes;
  aout_type = fdecl.tout_type
}
and alloc_data data = ADdata data
and alloc_class c = failwith "alloc_class: todo"

let alloc genv = List.map (alloc_decl genv)


(* PRODUCTION DU CODE *)
let round_16 a = match a mod 16 with
| 0 -> a
| i -> a + 16-i

(* fonction utilitaire pour déplacer une valeur de la pile vers la pile *)
let move_stack ofs1 ofs2 = 
  if ofs1 <> ofs2 then 
    movq (ind ~ofs:ofs1 rbp) (reg rdx) ++
    movq (reg rdx) (ind ~ofs:ofs2 rbp)
  else nop

let comparaison_count = ref 0
let lazy_count = ref 0
let if_count = ref 0
let branch_count = ref 0
let hardcoded_strings = ref []

let include_print_bool = ref false
let include_print_int = ref false
let include_div = ref false
let include_concat = ref false

let rec compile_decl = function 
| ADefn d -> compile_defn d
| ADfdecl d -> nop
| ADdata data -> nop
| _ -> failwith "compile_decl: todo"

and compile_defn (ident, patargs, aexpr, fpmax) = 
  if !dbg then Pretty.pp_aexpr std_formatter 0 aexpr ;
  label ident ++
  enter (imm (round_16 (abs fpmax))) ++

  compile_expr aexpr ++
  begin 
    match type_of_aexpr aexpr with
    | TUnit | TCons ("Effect", [TUnit]) -> movq (imm 0) (reg rax)
    | _ -> movq (ind ~ofs:(address_of_aexpr aexpr) rbp) (reg rax)
  end
  ++

  leave ++
  ret

and compile_expr = function 
| AEatom (at, t, res_adr) -> 
  compile_atom at ++
  move_stack (address_of_aatom at) res_adr
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
      | TInt -> include_print_int := true ; ".print_int"
      | TBool -> include_print_bool := true ; ".print_bool"
      | _ -> failwith "unsupported show"
    end
  | _ -> name
  end ++

  (* on remonte dans la pile *)
  addq (imm (round_16 (8*List.length params))) (reg rsp) ++
  (* on place le résultat là où demandé *)
  movq (reg rax) (ind ~ofs:res_adr rbp)
| AEbinop (e1, binop, e2, t, a) -> 
  begin 
    match binop with
    | Badd | Bsub | Bmul | Bdiv | Bconcat -> compile_binop (e1, binop, e2, t, a)
    | Band | Bor -> compile_lazy_binop (e1, binop, e2, t, a)
    | _ -> compile_binop_compare (e1, binop, e2, t, a)
  end
| AEunop (Uneg, aexpr, t, res_adr) ->
  compile_expr aexpr ++
  let eadr = address_of_aexpr aexpr in 
  movq (imm 0) (reg r8) ++
  subq (ind ~ofs:eadr rbp) (reg r8) ++
  movq (reg r8) (ind ~ofs:res_adr rbp)
| AEdo (expr_list, t, res_adr) ->
  List.fold_left (fun code expr -> code ++ compile_expr expr) nop expr_list ++
  movq (imm 0) (ind ~ofs:res_adr rbp)
| AEif (e1, e2, e3, _, res_adr) -> 
  let if_case_true = ".if_case_true_" ^ (string_of_int !if_count)
  and if_exit = ".if_exit_" ^ (string_of_int !if_count)
  in
  incr if_count ;
  compile_expr e1 ++
  movq (ind ~ofs:(address_of_aexpr e1) rbp) (reg rax) ++
  testq (reg rax) (reg rax) ++
  jnz if_case_true ++

  (* si la condition est fausse *)
  compile_expr e3 ++
  movq (ind ~ofs:(address_of_aexpr e3) rbp) (reg rax) ++
  movq (reg rax) (ind ~ofs:res_adr rbp)  ++
  jmp if_exit ++

  (* si la condition est vraie *)
  label if_case_true ++
  compile_expr e2 ++
  move_stack (address_of_aexpr e2) res_adr ++

  label if_exit

| AElet (abinding_list, aexpr, t, res_adr) -> 
  List.fold_left (fun code (badr, bexpr) -> 
    compile_expr bexpr ++
    movq (ind ~ofs:(address_of_aexpr bexpr) rbp) (reg rax) ++
    movq (reg rax) (ind ~ofs:badr rbp)  ++
    code
  ) nop abinding_list ++

  (* on compile l'expression dans le in, 
     et on met son résultat à la bonne position *)
  compile_expr aexpr ++
  move_stack (address_of_aexpr aexpr) res_adr
| AEcase (aexpr, blist, t, res_adr) -> 
  let branch_exit_label = ".branch_exit_" ^ (string_of_int !branch_count) in 
  incr branch_count ;
  compile_expr aexpr ++
  List.fold_left (fun code (bpattern, bexpr) ->
    compile_pattern (address_of_aexpr aexpr) res_adr bexpr branch_exit_label bpattern ++ code
  ) (movq (imm 0) (ind ~ofs:res_adr rbp)) blist ++
  label branch_exit_label

and compile_binop (e1, binop, e2, t, res_adr) =
  compile_expr e1 ++
  compile_expr e2 ++
  let a1, a2 = address_of_aexpr e1, address_of_aexpr e2 in 
  (* le cas de la division est un peu spécial *)
  if binop = Bdiv then begin
    include_div := true ;
    pushq (ind ~ofs:a2 rbp) ++
    pushq (ind ~ofs:a1 rbp) ++
    call ".div" ++
    movq (reg rax) (ind ~ofs:res_adr rbp)
  end
  else if binop = Bconcat then begin 
    include_concat := true ;
    pushq (ind ~ofs:a2 rbp) ++
    pushq (ind ~ofs:a1 rbp) ++
    call ".concat" ++
    movq (reg rax) (ind ~ofs:res_adr rbp)
  end
  else begin
    (* on choisit l'instruction assembleur correspondant au calcul *)
    let instruction = match binop with 
    | Badd -> addq
    | Bsub -> subq 
    | Bmul -> imulq
    | _ -> failwith "ce cas n'est pas sensé se produire"
    in 
    movq (ind ~ofs:a1 rbp) (reg r8) ++
    instruction (ind ~ofs:a2 rbp) (reg r8) ++
    movq (reg r8) (ind ~ofs:res_adr rbp)
  end

and compile_lazy_binop (e1, binop, e2, t, res_adr) = 
  let uid = string_of_int !lazy_count in 
  incr lazy_count ;
  let lazy_continue = ".lazy_continue_" ^ uid
  and lazy_end = ".lazy_end_" ^ uid in 
  let instruction, default = match binop with 
  | Bor -> jne, 1
  | Band -> je, 0
  | _ -> failwith ("ce cas est sensé avoir été traité par compile_binop. Opérateur : " ^ (Pretty.print_binop binop))
  in
  compile_expr e1 ++
  movq (ind ~ofs:(address_of_aexpr e1) rbp) (reg r8) ++
  testq (reg r8) (reg r8) ++
  instruction lazy_continue ++

  compile_expr e2 ++
  move_stack (address_of_aexpr e2) res_adr ++
  jmp lazy_end ++

  label lazy_continue ++
  movq (imm default) (ind ~ofs:res_adr rbp) ++
  
  label lazy_end

and compile_binop_compare (e1, binop, e2, t, res_adr) = 
  compile_expr e1 ++
  compile_expr e2 ++
  let a1, a2 = address_of_aexpr e1, address_of_aexpr e2 in 
  let expr_type = type_of_aexpr e1 in
  match binop with
  | Blt -> comparaison_code jl a1 a2 res_adr
  | Ble -> comparaison_code jle a1 a2 res_adr
  | Bgt | Bge -> failwith "la compilation des opérateurs binaire > et >= sont sensés être traités en utilisant la production de code de < et <="
  | Beq when expr_type = TInt -> comparaison_code je a1 a2 res_adr
  | Beq when expr_type = TBool -> comparaison_code je a1 a2 res_adr
  | Beq when expr_type = TUnit -> movq (imm 1) (ind ~ofs:res_adr rbp) (* toujours vrai *)
  | Beq when expr_type = TStr -> 
    let uid = string_of_int !comparaison_count in 
    let cmp_is_true = ".cmp_is_true_" ^ uid
    and cmp_is_false = ".cmp_is_false_" ^ uid in
    incr comparaison_count ;
    
    movq (ind ~ofs:a1 rbp) (reg rdi) ++
    movq (ind ~ofs:a2 rbp) (reg rsi) ++
    call "strcmp" ++
    testq (reg rax) (reg rax) ++

    jz cmp_is_true ++
    movq (imm 0) (ind ~ofs:res_adr rbp) ++
    jmp cmp_is_false ++

    label cmp_is_true ++
    movq (imm 1) (ind ~ofs:res_adr rbp) ++
    
    label cmp_is_false
  | Beq -> failwith "la comparaison doit être effectuée entre deux expressions entières ou booléennes."
  | _ -> failwith ("ce cas est sensé avoir été traité par compile_binop. Opérateur : " ^ (Pretty.print_binop binop))
and comparaison_code instruction a1 a2 res_adr =
  let uid = string_of_int !comparaison_count in 
  let cmp_is_true = ".cmp_is_true_" ^ uid
  and cmp_is_false = ".cmp_is_false_" ^ uid in
  incr comparaison_count ;
  
  movq (ind ~ofs:a1 rbp) (reg r13) ++
  movq (ind ~ofs:a2 rbp) (reg r12) ++
  cmpq (reg r12) (reg r13) ++

  instruction cmp_is_true ++
  movq (imm 0) (ind ~ofs:res_adr rbp) ++
  jmp cmp_is_false ++

  label cmp_is_true ++
  movq (imm 1) (ind ~ofs:res_adr rbp) ++
  
  label cmp_is_false

and compile_atom = function 
| AAexpr (expr, t, res_adr) -> 
  compile_expr expr ++
  move_stack res_adr (address_of_aexpr expr)
| AAconst (c, c_adr, t, res_adr) -> 
  compile_const c c_adr ++
  move_stack c_adr res_adr
| AAlident _ -> nop (* TODO : vérifier que ça marche bien comme ça *)
| AAuident (ident, t, addr) -> 
  movq (imm 8) (reg rdi) ++
  call "malloc" ++
  movq (imm ident) (ind rax) ++
  movq (reg rax) (ind ~ofs:addr rbp)

and compile_const c adr = 
  let ptr = match c with
  (* si c'est une constante facile, on la représente par un immédiat *)
  | Cint i -> imm i
  | Cbool false -> imm 0
  | Cbool true -> imm 1
  (* sinon, on l'ajoute au segment data, et on renvoie un label *)
  | Cstring s -> 
    let str_label = ".hardcoded_string_"^(string_of_int (List.length !hardcoded_strings)) in 
    hardcoded_strings := (str_label, s) :: !hardcoded_strings ;
    (ilab str_label)
  in
  movq ptr (ind ~ofs:adr rbp)

and compile_pattern condition_adr res_adr expr_true end_label = function 
| AParg (patarg) -> begin 
    match patarg with
    | APconst (c, adr) -> 
      let branch_true_label = ".branch_true_" ^ (string_of_int !branch_count) in 
      incr branch_count ;

      compile_const c (failwith "adress") ++
      movq (ind ~ofs:condition_adr rbp) (reg r8) ++
      movq (ind ~ofs:(failwith "find address of const") rbp) (reg r9) ++
      cmpq (reg r8) (reg r9) ++
      
      (* si le pattern ne match pas, on va au prochain cas *)
      jne branch_true_label ++
      (* sinon, on exécute l'expression de la branche *)
      compile_expr expr_true ++
      move_stack (address_of_aexpr expr_true) res_adr ++
      jmp end_label ++ (* et on finit *)
      label branch_true_label
    | APlident (id, adr) -> 
        move_stack condition_adr adr ++
        compile_expr expr_true ++
        move_stack (address_of_aexpr expr_true) res_adr ++
        jmp end_label
    | APuident (uid, adr) ->
        let branch_true_label = ".branch_true_" ^ (string_of_int !branch_count) in 
        incr branch_count ;
        movq (ind ~ofs:condition_adr rbp) (reg r8) ++
        cmpq (imm uid) (ind r8)++
        jne end_label ++
        compile_expr expr_true ++
        move_stack (address_of_aexpr expr_true) res_adr ++
        jmp end_label ++
        label branch_true_label
    end
| APconsarg (id, plist) -> failwith "compile_pattern: todo APconsarg"

let data_count = ref 0
let compile_program (p : tdecl list) ofile dbg_mode =
  dbg := dbg_mode ;
  (* ajout des data à l'environnement global *)
  let genv = ref Smap.empty in
  List.iter (function
  | TDdata data -> 
    let data_constr = ref Smap.empty in
    List.iter (fun (ident, t) -> 
      let constr = (!data_count, List.length t) in
      incr data_count ;
      data_constr := Smap.add ident constr !data_constr) data.types ;
      
      genv := Smap.union (fun _ -> 
        failwith "deux constructions de data ont le même nom. Cette erreur devrait être soulevée au typage."
      ) !genv !data_constr
  | _ -> ()
  ) p ;

  let p = alloc !genv p in
  let code = List.fold_left (fun code tdecl -> code ++ compile_decl tdecl) nop p in
  
  let data = Hashtbl.fold (fun x _ l -> label x ++ dquad [1] ++ l) (Hashtbl.create 17)
  begin
    label ".Sprint_int" ++ string "%d\n" ++
    label ".Sprint_string" ++ string "%s\n" ++
    label "true" ++ string "true" ++
    label "false" ++ string "false" ++
    List.fold_left (fun code (label_name, str) ->
      code ++ label label_name ++ string str
    ) nop (List.rev !hardcoded_strings)
  end
  in

  
  let p =
    { text =
        globl "main" ++
        code ++

        (* afficheur d'entiers *)
        begin if !include_print_int then begin (* n'ajoute le code de la fonction que s'il est réellement utilisé *)
        (* TODO : personnaliser *)
        label ".print_int" ++
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
        ret 
        end else nop end ++


        (* fonction log de purescript *)
        label "log" ++
        enter (imm 0) ++
        movq (ind ~ofs:16 rbp) (reg rsi) ++
        movq (ilab ".Sprint_string") (reg rdi) ++
        xorq (reg rax) (reg rax) ++
        call "printf" ++
        leave ++
        ret ++

        (* la fonction pure de purescript *)
        label "pure" ++
        enter (imm 0) ++
        movq (imm 0) (reg rax) ++ (* retourne toujours 0 *)
        leave ++
        ret ++

        (* fonction not de purescript *)
        label "not" ++
        enter (imm 0) ++
        movq (ind ~ofs:16 rbp) (reg rax) ++
        (* si la valeur vaut 0, on la remplace par 1, sinon par 0 *)
        testq (reg rax) (reg rax) ++
        jz ".not_0" ++
        movq (imm 0) (reg rax) ++
        leave ++
        ret ++
        label ".not_0" ++
        movq (imm 1) (reg rax) ++
        leave ++
        ret ++

        (* FONCTIONS UTILITAIRES *)

        (* fonction de division *)
        (* la complexité vient du fait que idivq peut retourner 
          un résultat négatif, ce que l'on ne veut pas (cf. arith2.out) *)
        begin if !include_div then begin
        label ".div" ++
        enter (imm 0) ++
        movq (ind ~ofs:16 rbp) (reg rax) ++
        (* on place les drapeaux pour tester le signe de rax *)
        testq (reg rax) (reg rax) ++
        (* si négatif, on appelle la fonction dédiée *)
        js ".div_s" ++
        movq (ind ~ofs:24 rbp) (reg rcx) ++
        movq (imm 0) (reg rdx) ++
        idivq (reg rcx) ++
        leave ++
        ret ++

        label ".div_s" ++
        movq (ind ~ofs:24 rbp) (reg rcx) ++
        movq (imm (-1)) (reg rdx) ++
        idivq (reg rcx) ++
        (* on teste si rdx est non-nul *)
        testq (reg rdx) (reg rdx) ++
        jnz ".div_1" ++
        leave ++
        ret ++

        (* si rdx est non-nul *)
        label ".div_1" ++
        (* si le quotient est positif, on retire 1 *)
        cmpq (imm 0) (ind ~ofs:24 rbp) ++
        jg ".decq_leave" ++
        incq (reg rax) ++
        leave ++
        ret ++

        label ".decq_leave" ++
        decq (reg rax) ++
        leave ++
        ret
        end else nop end ++

        (* fonction mod de purescript *)
        (* là encore, la complexité vient du fait que idivq peut retourner 
          un résultat négatif, ce que l'on ne veut pas (cf. arith2.out) *)
        (* "l'algorithme" est le même que pour _div *)
        (*label "mod" ++
        enter (imm 0) ++
        movq (ind ~ofs:16 rbp) (reg rax) ++
        movq (ind ~ofs:24 rbp) (reg rbx) ++
        cqto ++ 
        idivq !%rbx ++
        (* on met le résultat dans rax, qui sera ensuite 
           recopié sur la pile par l'instruction suivante (cf. compile_expr)*)
        movq (reg rdx) (reg rax) ++
        leave ++
        ret ++*)

        label "mod" ++
        enter (imm 0) ++
        movq (ind ~ofs:16 rbp) (reg rax) ++
        testq (reg rax) (reg rax) ++

        js ".mod_s" ++
        movq (ind ~ofs:24 rbp) (reg rcx) ++
        movq (imm 0) (reg rdx) ++
        idivq (reg rcx) ++
        movq (reg rdx) (reg rax) ++
        leave ++
        ret ++

        label ".mod_s" ++
        movq (ind ~ofs:24 rbp) (reg rcx) ++
        movq (imm (-1)) (reg rdx) ++
        idivq (reg rcx) ++
        (*testq (reg rdx) (reg rdx) ++*)
        movq (reg rdx) (reg rax) ++
        testq (reg rax) (reg rax) ++
        jz ".mod_0" ++
        cmpq (imm 0) (ind ~ofs:24 rbp) ++
        js ".mod_ss" ++
        addq (ind ~ofs:24 rbp) (reg rax) ++
        leave ++
        ret ++

        label ".mod_ss" ++
        subq (ind ~ofs:24 rbp) (reg rax) ++
        leave ++
        ret ++

        label ".mod_0" ++
        movq (imm 0) (reg rax) ++
        leave ++
        ret ++

        (* afficheur de booléen *)
        begin if !include_print_bool then begin
        label ".print_bool" ++
        enter (imm 0) ++
        cmpq (imm 0) (ind ~ofs:16 rbp) ++
        je ".Lfalse" ++
        movq (ilab "true") (reg rax) ++
        leave ++
        ret ++
        
        label ".Lfalse" ++
        movq (ilab "false") (reg rax) ++
        leave ++
        ret
        end else nop end ++

        (* fonction de concaténation de deux strings *)
        begin if !include_concat then begin
        label ".concat" ++
        enter (imm 0) ++
        (* calcul de la longueur de la première chaîne *)
        movq (ind ~ofs:16 rbp) (reg rdi) ++
        call "strlen" ++
        movq (reg rax) (reg r9) ++
        movq (ind ~ofs:24 rbp) (reg rdi) ++
        (* calcul de la longueur de la seconde chaîne *)
        call "strlen" ++
        addq (reg rax) (reg r9) ++ (* longeur totale*)
        incq (reg r9) ++
        movq (reg r9) (reg rdi) ++
        (* allocation de la mémoire nécessaire à s1 <> s2 *)
        call "malloc" ++
        (* déplacement puis concaténation *)
        movq (reg rax) (reg r8) ++
        movq (reg rax) (reg rdi) ++
        movq (ind ~ofs:16 rbp) (reg rsi) ++
        call "strcpy" ++
        movq (ind ~ofs:24 rbp) (reg rsi) ++
        call "strcat" ++
        movq (reg r8) (reg rax) ++
        leave ++
        ret
        end else nop end
      ;
      data = data
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f
