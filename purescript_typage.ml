open Purescript_ast
(*
type file =
  {imports : imports; decls : decl list}

and imports = Import

and decl =
  | Ddefn of defn
  | Dtdecl of tdecl
  | Ddata of ident * (ident list) * ((ident * (atype list)) list)
  | Dclass of ident * (ident list) * tdecl list
  | Dinstance of instance * defn list

and defn =
  {ident : ident; patargs : patarg list; expr : expr}

and tdecl =
  {dident : ident; identlist : ident list; ntypelist : ntype list; purtypelist : purtype list; purtype : purtype}

and ntype =
  {nident : ident; atypes : atype list}

and atype =
  | Aident of ident
  | Apurtype of purtype

and purtype =     (*remplace type car type est un mot clef en Ocaml*)
  | Patype of atype
  | Pntype of ntype

and instance =
  | Intype of ntype
  | Iarrow of ntype * ntype
  | Imularrow of ntype list * ntype

and patarg =
  | Pconstant of constant
  | Plident of ident
  | Puident of ident
  | Ppattern of pattern

and pattern =
  | Ppatarg of patarg
  | Pmulpatarg of ident * patarg list

and constant =
  | Cbool of bool
  | Cint of int
  | Cstring of string

and atom =
  | Aconstant of constant
  | Alident of ident
  | Auident of ident
  | Aexpr of expr
  | Aexprtype of expr * purtype

and expr =
  | Eatom of atom
  | Ebinop of binop * expr * expr
  | Elident of ident * atom list 
  | Euident of ident * atom list 
  | Eif of expr * expr * expr
  | Edo of expr list
  | Elet of binding list * expr
  | Ecase of expr * branch list

and binding =
  {ident : ident; bindexpr : expr}

and branch =
  {pattern : pattern; expr : expr}

and binop = Bequals | Bnotequals | Binf | Binfeq | Bsup | Bsupeq | Bplus | Bminus | Btimes | Bdivide | Band | Bor | Bcons


and ident = string
*)

type typ =
  | Unit
  | Int
  | String
  | Boolean
  | Tcustom of string * typ list
  | Tarrow of typ list * typ
  | Tvar of tvar
  | Tgeneral of string

and tvar = { id : int; mutable def : typ option }

type typfunctions = {
  flidents : ident list;
  instancelist : typinstance list;
  typlist : typ list;
  typ : typ;
  matching : (motif list * expr) list;
}

and typinstance = {
  typinstancelist : typinstance list;
  typlist : typ list;
  matching : (ident * motif list * expr) list;
}

and motif = Mconst of constant | Mident of string

and typclass = {
  variablesdetypes : string list;
  fonctionsdeclasse : (string * typ list) list;
}

module Smap = Map.Make (String)

exception TypingError of string

let typingerror s = raise (TypingError s)

let smapfind s smap =
  if Smap.mem s smap then Smap.find s smap
  else typingerror ("L'identificateur " ^ s ^ " n'est pas défini")

let string_to_list s = List.init (String.length s) (String.get s)
let isalident s = List.mem s.[0] (string_to_list "azertyuiopqsdfghjklmwxcvbn")

let (globalenvinstances : (typ list * (ident * typ list) list) list Smap.t ref)
    =
  ref (Smap.add "Show" [ ([ Int ], []); ([ Boolean ], []) ] Smap.empty)

let print_bool b = if b then print_endline "true" else print_endline "false"

(*
let globalenvfunctions = Smap.add "not" {flidents = []; instancelist = []; typlist = [Boolean];typ = Boolean;matching = []} Smap.empty
let globalenvfunctions = Smap.add "mod" {flidents = []; instancelist = []; typlist = [Int;Int];typ = Int;matching = []} globalenvfunctions
let globalenvfunctions = Smap.add "log" {flidents = []; instancelist = []; typlist = [String];typ = Effect(Unit);matching = []} globalenvfunctions
let globalenvfunctions = Smap.add "pure" {flidents = ["a"]; instancelist = []; typlist = [Tenv "a"];typ = Effect(Tenv "a");matching = []} globalenvfunctions

let globalenvclasses = Smap.add "Show" {variablesdetypes = ["a"]; fonctionsdeclasse = [("show",[Tenv "a";String])]} Smap.empty
let globalenvfunctions = Smap.add "show" {flidents = ["a"]; instancelist = []; typlist = [Tenv "a"];typ = String;matching = []} globalenvfunctions

let globalenvinstances = Smap.add "Show" [([Boolean],{typinstancelist = []; typlist = [Boolean]; matching = []});([Int],{typinstancelist = []; typlist = [Int]; matching = []})]
*)

module V = struct
  type t = tvar

  let compare v1 v2 = Stdlib.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id

  let create =
    let r = ref 0 in
    fun () ->
      incr r;
      { id = !r; def = None }
end

let rec head t =
  match t with Tvar { id = _; def = Some t2 } -> head t2 | _ -> t

let rec canon t =
  match t with
  | Tvar { id = _; def = Some t2 } -> head t2
  | Tarrow (tlist, t2) -> Tarrow (List.map canon tlist, canon t2)
  | _ -> t

let rec print_typ t =
  match canon t with
  | Int -> print_string "Int\n"
  | String -> print_string "String\n"
  | Unit -> print_string "Unit\n"
  | Boolean -> print_string "Boolean\n"
  | Tarrow (tlist, t) ->
      print_string "(";
      List.iter print_typ tlist;
      print_string ") -> ";
      print_typ t;
      print_string "\n"
  | Tcustom (s, tlist) ->
      print_string s;
      print_string " of (";
      List.iter print_typ tlist;
      print_string ")\n"
  | Tvar { id; def = None } ->
      print_int id;
      print_string " polymorphe\n"
  | Tgeneral s -> print_endline s
  | _ -> failwith "Non"

exception UnificationFailure of typ * typ

let unification_error t1 t2 = raise (UnificationFailure (canon t1, canon t2))
let rec grosor l = match l with [] -> false | x :: q -> x || grosor q

let rec occur tv t =
  match head t with
  | Tvar tv2 -> V.equal tv tv2
  | Tarrow (tlist, t2) -> grosor (List.map (occur tv) tlist) || occur tv t2
  | _ -> false

let unifyable tlist1 tlist2 =
  print_int (List.length tlist1);
  let (assoc : typ Smap.t ref) = ref Smap.empty in
  let rec aux tlist1 tlist2 =
    print_endline "ok";
    match (tlist1, tlist2) with
    | [], [] -> true
    | Unit :: q1, Unit :: q2
    | Int :: q1, Int :: q2
    | String :: q1, String :: q2
    | Boolean :: q1, Boolean :: q2 ->
        print_endline "c'est pareil";
        print_typ (List.hd tlist1);
        print_typ (List.hd tlist2);
        aux q1 q2
    | Tcustom (s, _) :: q1, Tcustom (t, _) :: q2 when s = t -> aux q1 q2
    | Tgeneral s :: q1, t :: q2 ->
        print_endline "c'est pas pareil 1";
        if Smap.mem s !assoc then aux (Smap.find s !assoc :: q1) (t :: q2)
        else (
          assoc := Smap.add s t !assoc;
          aux q1 q2)
    | t :: q1, Tgeneral s :: q2 ->
        print_endline "c'est pas pareil 2";
        aux tlist2 tlist1
    | _ ->
        print_endline "c'est pas pareil";
        false
  in
  aux tlist1 tlist2

module Vset = Set.Make (V)

let rec fvars t =
  match head t with
  | Unit | Int | String | Boolean -> Vset.empty
  | Tcustom (_, tlist) ->
      List.fold_left Vset.union Vset.empty (List.map fvars tlist)
  | Tarrow (tlist, t2) ->
      Vset.union
        (List.fold_left Vset.union Vset.empty (List.map fvars tlist))
        (fvars t2)
  | Tvar tv -> Vset.singleton tv
  | Tgeneral s -> Vset.empty

type schema = { vars : Vset.t; typ : typ }
type env = { bindings : schema Smap.t; fvars : Vset.t }

let empty = { bindings = Smap.empty; fvars = Vset.empty }

let add s t envi =
  if s = "_" then envi
  else
    {
      bindings = Smap.add s { vars = Vset.empty; typ = t } envi.bindings;
      fvars = Vset.union envi.fvars (fvars t);
    }

let envunion f env1 env2 =
  {
    bindings = Smap.union f env1.bindings env2.bindings;
    fvars = Vset.union env1.fvars env2.fvars;
  }

let grossunion s =
  Vset.fold (fun v s -> Vset.union (fvars (Tvar v)) s) s Vset.empty

let add_gen s t envi =
  if s = "_" then envi
  else
    let vt = fvars t in
    let bindings = { vars = Vset.diff vt (grossunion envi.fvars); typ = t } in
    { bindings = Smap.add s bindings envi.bindings; fvars = envi.fvars }

module Vmap = Map.Make (V)

let find x env =
  let tx = smapfind x env.bindings in
  let s =
    Vset.fold (fun v s -> Vmap.add v (Tvar (V.create ())) s) tx.vars Vmap.empty
  in
  let rec subst t =
    match head t with
    | Tvar x as t -> ( try Vmap.find x s with Not_found -> t)
    | Int | String | Unit | Boolean -> t
    | Tcustom (s, tlist) -> Tcustom (s, List.map subst tlist)
    | Tarrow (tlist, t2) -> Tarrow (List.map subst tlist, subst t2)
    | Tgeneral s -> Tgeneral s
  in
  subst tx.typ

let mem x env = Smap.mem x env.bindings

type constructor = {
  cident : string;
  ctlist : typ list;
  ctyp : typ;
  cenvvartyps : (typ * int) Smap.t;
}

let (envconstructors : constructor Smap.t ref) = ref Smap.empty

let rec smapaddlist env l =
  match l with
  | [], [] -> env
  | s :: q1, t :: q2 -> smapaddlist (Smap.add s t env) (q1, q2)
  | _ -> failwith "bah non"

let rec substitute dejapris t =
  match head t with
  | Tgeneral s -> smapfind s dejapris
  | Tcustom (s, tlist) -> Tcustom (s, List.map (substitute dejapris) tlist)
  | _ -> t

let lastdefined = ref ""
let (deflist : defn list ref) = ref []

let smaptolist sm =
  let rec aux s x l = (s, x) :: l in
  Smap.fold aux sm []

let envfonctions =
  ref
    (smapaddlist Smap.empty
       ( [ "not"; "mod"; "log"; "pure"; "show" ],
         [
           (Tarrow ([ Boolean ], Boolean), Smap.empty, None, Smap.empty);
           (Tarrow ([ Int; Int ], Int), Smap.empty, None, Smap.empty);
           ( Tarrow ([ String ], Tcustom ("Effect", [ Unit ])),
             Smap.empty,
             None,
             Smap.empty );
           ( Tarrow ([ Tgeneral "a" ], Tcustom ("Effect", [ Tgeneral "a" ])),
             Smap.add "a" (Tgeneral "a", 0) Smap.empty,
             None,
             Smap.empty );
           ( Tarrow ([ Tgeneral "a" ], String),
             Smap.add "a" (Tgeneral "a", 0) Smap.empty,
             Some "Show",
             Smap.empty );
         ] ))

let (envclasses : (ident list * typ Smap.t * typ list) Smap.t ref) =
  ref
    (Smap.add "Show"
       ( [ "a" ],
         Smap.add "show" (Tarrow ([ Tgeneral "a" ], String)) Smap.empty,
         [] )
       Smap.empty)

let print_smap sm =
  let rec aux s sch =
    print_endline s;
    print_typ sch.typ
  in
  Smap.iter aux sm

let fstr (a, _, _, _) = a
let sndr (_, b, _, _) = b
let thrd (_, _, c, _) = c
let fourth (_, _, _, d) = d

let rec alldifferent l =
  match l with [] -> true | x :: q -> (not (List.mem x q)) && alldifferent q

let rec grosand l = match l with [] -> true | b :: q -> b && grosand q

let rec typfile f =
  let env = add "unit" Unit empty in
  let envtyps =
    ref
      (smapaddlist Smap.empty
         ( [ "Int"; "String"; "Unit"; "Boolean"; "Effect" ],
           [
             (Int, 0);
             (String, 0);
             (Unit, 0);
             (Boolean, 0);
             (Tcustom ("Effect", [ Unit ]), 1);
           ] ))
  in
  List.iter (typdecl env envtyps !globalenvinstances) f.decls;
  checkforenddef !envtyps !globalenvinstances;
  if not (Smap.mem "main" !envfonctions) then
    typingerror "Pas de fonction main définie"
  else ()

and typdecl env envtyps envinstances d =
  match d with
  | Dtdecl td ->
      checkforenddef !envtyps !globalenvinstances;
      let a, b, d = typtdecl env !envtyps Smap.empty td in
      envfonctions := Smap.add td.dident (a, b, None, d) !envfonctions
  | Ddefn df -> (
      print_endline "je suis le probleme ?";
      match fstr (smapfind df.ident !envfonctions) with
      | Tarrow (tlist, t) ->
          typdfn env !envtyps !globalenvinstances true df tlist t;
          print_endline "Non"
      | _ -> failwith "pas possible")
  | Ddata (s, slist, ialist) ->
      checkforenddef !envtyps !globalenvinstances;
      typdata env envtyps envinstances s slist ialist
  | Dclass (s, slist, tdlist) ->
      checkforenddef !envtyps !globalenvinstances;
      typclass env !envtyps !globalenvinstances s slist tdlist
  | Dinstance (i, deflist) ->
      checkforenddef !envtyps !globalenvinstances;
      typinstance env !envtyps !globalenvinstances i deflist

and typexpr env envtyps
    (envinstances : (typ list * (ident * typ list) list) list Smap.t)
    (general : bool) expr =
  match expr with
  | Ebinop (b, e1, e2) -> (
      match b with
      | Bequals | Bnotequals ->
          let t = typexpr env envtyps envinstances general e1 in
          if List.mem t [ Int; String; Boolean; Unit ] then
            if typexpr env envtyps envinstances general e2 <> t then
              typingerror "Mauvais type"
            else Boolean
          else typingerror "Mauvais type"
      | Binf | Binfeq | Bsup | Bsupeq ->
          if typexpr env envtyps envinstances general e1 <> Int then
            typingerror "Mauvais type"
          else if typexpr env envtyps envinstances general e2 <> Int then
            typingerror "Mauvais type"
          else Boolean
      | Bplus | Bminus | Btimes | Bdivide ->
          if typexpr env envtyps envinstances general e1 <> Int then
            typingerror "Mauvais type"
          else if typexpr env envtyps envinstances general e2 <> Int then
            typingerror "Mauvais type"
          else Int
      | Bor | Band ->
          if typexpr env envtyps envinstances general e1 <> Boolean then
            typingerror "Mauvais type"
          else if typexpr env envtyps envinstances general e2 <> Boolean then
            typingerror "Mauvais type"
          else Boolean
      | Bcons ->
          if typexpr env envtyps envinstances general e1 <> String then
            typingerror "Mauvais type"
          else if typexpr env envtyps envinstances general e2 <> String then
            typingerror "Mauvais type"
          else String)
  | Eatom a -> typatom env envtyps envinstances general a
  | Eif (e1, e2, e3) ->
      if typexpr env envtyps envinstances general e1 <> Boolean then
        typingerror "Mauvais type"
      else
        let t = typexpr env envtyps envinstances general e2 in
        if typexpr env envtyps envinstances general e3 <> t then
          typingerror "Mauvais type"
        else t
  | Edo elist ->
      List.iter
        (fun e ->
          if
            typexpr env envtyps envinstances general e
            <> Tcustom ("Effect", [ Unit ])
          then typingerror "Mauvais type"
          else ())
        elist;
      Tcustom ("Effect", [ Unit ])
  | Elet (blist, e) ->
      let rec aux env envtyps envinstances l =
        match l with
        | b :: q ->
            let t = typexpr env envtyps envinstances general b.bindexpr in
            aux (add_gen b.ident t env) envtyps envinstances q
        | [] -> env
      in
      typexpr
        (aux env envtyps envinstances blist)
        envtyps envinstances general e
  | Ecase (e, blist) -> (
      let t = typexpr env envtyps envinstances general e in
      match blist with
      | [] -> typingerror "Pattern vide"
      | b :: q ->
          let t' = typbranch env envtyps envinstances t general b in
          List.iter
            (fun x ->
              if typbranch env envtyps envinstances t general x <> t' then
                typingerror "bad pattern type"
              else ())
            q;
          if
            not
              (checkexaustivelist env envtyps envinstances [ t ]
                 (List.map
                    (fun x -> [ x ])
                    (List.map (fun b -> b.pattern) blist)))
          then typingerror "Pattern non exhaustif"
          else t')
  | Elident (f, alist) ->
      let envinstances =
        Smap.union noconseqconflit envinstances
          (fourth (smapfind f !envfonctions))
      in
      if mem f env then typingerror (f ^ " n'est pas une fonction")
      else (
        (match thrd (smapfind f !envfonctions) with
        | None -> ()
        | Some cident ->
            let instances = smapfind cident envinstances in
            find_compatible_instance envinstances cident f general instances
              (List.map (typatom env envtyps envinstances general) alist));
        let tlist, t =
          match fstr (smapfind f !envfonctions) with
          | Tarrow (tlist, t) -> (tlist, t)
          | _ -> failwith "pas possible"
        in
        let dejapris = ref Smap.empty in
        let rec aux tlist alist =
          match (tlist, alist) with
          | [], [] -> ()
          | t :: q1, a :: q2 -> (
              match t with
              | Tgeneral s -> (
                  let t' = typatom env envtyps envinstances general a in
                  match t' with
                  | Tgeneral _ -> aux q1 q2
                  | _ ->
                      if Smap.mem s !dejapris && t' <> smapfind s !dejapris then
                        typingerror
                          ("types incompatibles entre eux dans l'appel de " ^ f)
                      else dejapris := Smap.add s t' !dejapris;
                      aux q1 q2)
              | _ ->
                  if typatom env envtyps envinstances general a <> t then
                    typingerror ("mauvais argument de fonction pour " ^ f)
                  else aux q1 q2)
          | _ ->
              typingerror
                ("la fonction " ^ f ^ " ne prend pas autant d'arguments")
        in
        aux tlist alist;
        substitute !dejapris t)
  | Euident (s, alist) ->
      let constr = smapfind s !envconstructors in
      let rec aux alist tlist =
        match (alist, tlist) with
        | [], [] -> ()
        | a :: q1, t :: q2 ->
            if typatom env envtyps envinstances general a <> t then
              typingerror
                ("types pas compatible dans l'utilisation du constructeur " ^ s)
            else aux q1 q2
        | _ ->
            typingerror
              ("La liste d'atome n'est pas de la bonne longueur dans \
                l'utilisation du constructeur " ^ s)
      in
      aux alist constr.ctlist;
      constr.ctyp

and find_compatible_instance envinstances cident f (general : bool) instances
    tlist =
  let slist, classenvfonctions, flist = smapfind cident !envclasses in
  let ftlist =
    match smapfind f classenvfonctions with
    | Tarrow (tlist, t) -> tlist
    | _ -> failwith "pas possible"
  in
  let rec constrsmap sm tlist1 tlist2 =
    match (tlist1, tlist2) with
    | [], [] -> sm
    | Tgeneral s :: q1, t :: q2 -> Smap.add s t (constrsmap sm q1 q2)
    | t1 :: q1, t2 :: q2 when t1 = t2 -> constrsmap sm q1 q2
    | _ ->
        typingerror
          ("L'appel de " ^ f
         ^ " n'est pas compatible avec sa définition dans la classe " ^ cident)
  in
  let sm = constrsmap Smap.empty ftlist tlist in
  let listdestvar = List.map (fun s -> smapfind s sm) slist in
  let rec compatible tlist1 tlist2 =
    match (tlist1, tlist2) with
    | t1 :: q1, t2 :: q2 when t1 = t2 ->
        print_endline "pas de probleme";
        compatible q1 q2
    | [], [] -> true
    | t1 :: q1, t2 :: q2 ->
        print_endline "probleme";
        print_typ t1;
        print_typ t2;
        false
    | _ -> false
  in
  let rec linstanceestdispo envinstances tlist ilist =
    match ilist with
    | [] -> false
    | tl :: q ->
        ((compatible tl tlist && not general) || (unifyable tl tlist && general))
        || linstanceestdispo envinstances tlist q
  in
  let rec touteslesinstancessontdispos envinstances averif =
    match averif with
    | [] -> true
    | (s, tlist) :: q ->
        linstanceestdispo envinstances tlist
          (List.map fst (smapfind s envinstances))
  in
  let rec cherchercompatible tlistlist tlist =
    match tlistlist with
    | [] ->
        typingerror
          ("Pas d'instance compatible pour la classe " ^ cident
         ^ " dans l'appel de " ^ f)
    | tl :: q
      when ((compatible (fst tl) tlist && not general)
           || (unifyable (fst tl) tlist && general))
           && touteslesinstancessontdispos envinstances (snd tl) ->
        ()
    | tl :: q ->
        print_endline "on veut pas de ";
        List.iter print_typ (fst tl);
        print_bool (unifyable (fst tl) tlist);
        cherchercompatible q tlist
  in
  cherchercompatible instances listdestvar

and typatom env envtyps envinstances (general : bool) a =
  match a with
  | Alident s -> find s env
  | Auident s -> (smapfind s !envconstructors).ctyp
  | Aconstant c -> typconstant env envtyps envinstances c
  | Aexpr e -> typexpr env envtyps envinstances general e
  | Aexprtype (e, p) ->
      let t = typexpr env envtyps envinstances general e in
      if t <> typpurtyp env envtyps envinstances p then
        typingerror "Mauvais type"
      else t

and typconstant env envtyps envinstances c =
  match c with Cbool _ -> Boolean | Cint _ -> Int | Cstring _ -> String

and typbranch env envtyps envinstances t (general : bool) b =
  let conflit s a _ = Some a in
  let env =
    envunion conflit
      (ensuretyppattern empty envtyps envinstances t b.pattern)
      env
  in
  typexpr env envtyps envinstances general b.expr

and ensuretyppattern env envtyps envinstances t p =
  match p with
  | Ppatarg p -> ensuretyppatarg env envtyps envinstances t p
  | Pmulpatarg (s, plist) ->
      let constr = smapfind s !envconstructors in
      if constr.ctyp <> t then
        typingerror (s ^ "n'est pas un constructeur du bon type")
      else
        let rec aux envi plist tlist =
          match (plist, tlist) with
          | [], [] -> envi
          | p :: q1, t :: q2 ->
              let a = ensuretyppatarg env envtyps envinstances t p in
              let conflit s _ _ =
                typingerror ("l'ident " ^ s ^ " est utilisé plusieurs fois")
              in
              let env = aux envi q1 q2 in
              {
                bindings = Smap.union conflit a.bindings env.bindings;
                fvars = Vset.union a.fvars env.fvars;
              }
          | _ ->
              typingerror
                ("pas la bonne taille de pattern dans l'utilisation du \
                  constructeur " ^ constr.cident)
        in
        aux env plist constr.ctlist

and ensuretyppatarg env envtyps envinstances t p =
  match p with
  | Pconstant c ->
      if typconstant env envtyps envinstances c <> t then
        typingerror "Mauvais patterne"
      else env
  | Plident s -> add s t env
  | Puident s ->
      if (smapfind s !envconstructors).ctyp <> t then (
        print_endline "devait etre un ";
        print_typ t;
        print_endline "et pas un ";
        print_typ (smapfind s !envconstructors).ctyp;
        typingerror (s ^ " n'est pas un constructeur du bon type"))
      else env
  | Ppattern p -> ensuretyppattern env envtyps envinstances t p

and checkexaustive env envtyps envinstances t plist =
  let rec contientident l =
    match l with
    | [] -> false
    | Ppatarg (Plident _) :: _ -> true
    | Ppatarg (Ppattern p) :: q -> contientident [ p ] || contientident q
    | _ :: q -> contientident q
  in
  let rec contientbool b l =
    match l with
    | [] -> false
    | Ppatarg (Pconstant (Cbool x)) :: _ when x = b ->
        print_endline ("y a eu un " ^ string_of_bool b);
        true
    | Ppatarg (Ppattern p) :: q -> contientbool b [ p ] || contientbool b q
    | _ :: q -> contientbool b q
  in
  let rec contientconstr cs l =
    match l with
    | [] -> false
    | Pmulpatarg (s, _) :: _ when s = cs -> true
    | Ppatarg (Puident s) :: _ when s = cs -> true
    | Ppatarg (Ppattern p) :: q ->
        contientconstr cs [ p ] || contientconstr cs q
    | _ :: q -> contientconstr cs q
  in
  if contientident plist then true
  else
    match t with
    | Boolean ->
        print_endline (string_of_bool (contientbool false plist));
        contientbool true plist && contientbool false plist
    | Tcustom (s, []) ->
        grosand
          (List.map
             (fun (_, constr) ->
               (not (constr.ctyp = t)) || contientconstr constr.cident plist)
             (smaptolist !envconstructors))
    | _ -> false

and checkexaustivelist env envtyps envinstances tlist plistlist =
  let rec isaident p =
    match p with
    | Ppatarg (Plident _) -> true
    | Ppatarg (Ppattern p) -> isaident p
    | _ -> false
  in
  let rec isabool b p =
    match p with
    | Ppatarg (Pconstant (Cbool x)) when x = b -> true
    | Ppatarg (Ppattern p) -> isabool b p
    | _ -> false
  in
  let rec isaconstr c p =
    match p with
    | Ppatarg (Puident s) when s = c -> true
    | Pmulpatarg (s, _) when s = c -> true
    | Ppatarg (Ppattern p) -> isaconstr c p
    | _ -> false
  in
  let rec contientidentlist f i l =
    match l with
    | [] -> []
    | x :: q when f (List.nth x i) -> x :: contientidentlist f i q
    | _ :: q -> contientidentlist f i q
  in
  let rec getthelists l =
    match l with
    | [] -> []
    | (Pmulpatarg (_, x) :: _) :: q ->
        List.map (fun t -> Ppatarg t) x :: getthelists q
    | (Ppatarg (Puident _) :: _) :: q -> getthelists q
    | (Ppatarg (Ppattern p) :: a) :: q -> getthelists ((p :: a) :: q)
    | _ -> typingerror "bah alors mais d'ou ca arrive ca"
  in
  let rec aux i l =
    if l = [] then false
    else if i >= List.length tlist then true
    else
      match List.nth tlist i with
      | Boolean ->
          aux (i + 1) (contientidentlist isaident i l)
          || aux (i + 1) (contientidentlist (isabool true) i l)
             && aux (i + 1) (contientidentlist (isabool false) i l)
      | Tcustom (s, []) ->
          aux (i + 1) (contientidentlist isaident i l)
          || grosand
               (List.map
                  (fun (_, constr) ->
                    (not (constr.ctyp = List.nth tlist i))
                    || aux (i + 1)
                         (contientidentlist (isaconstr constr.cident) i l)
                       && checkexaustivelist env envtyps envinstances
                            constr.ctlist
                            (getthelists
                               (contientidentlist (isaconstr constr.cident) i l)))
                  (smaptolist !envconstructors))
      | _ -> aux (i + 1) (contientidentlist isaident i l)
  in
  List.length tlist = 0 || aux 0 plistlist

and typpurtyp env envtyps envinstances pt =
  match pt with
  | Patype a -> typatype env envtyps envinstances a
  | Pntype n -> typntype env envtyps envinstances n

and typntype env envtyps envinstances n =
  let t, arite = smapfind n.nident envtyps in
  if List.length n.atypes <> arite then typingerror "pas la bonne arité"
  else
    match t with
    | Tcustom (s, _) ->
        Tcustom (s, List.map (typatype env envtyps envinstances) n.atypes)
    | _ -> t

and typatype (env : env) envtyps envinstances a =
  match a with
  | Apurtype p -> typpurtyp env envtyps envinstances p
  | Aident s ->
      print_endline ("On cherche ici " ^ s);
      let t, arite = smapfind s envtyps in
      if arite = 0 then t else typingerror "devrait etre un type d'arite 0"

and typtdecl env envtyps envinstances td =
  if Smap.mem td.dident !envfonctions then
    typingerror (td.dident ^ " est définie 2 fois")
  else if not (alldifferent td.identlist) then
    typingerror
      ("toutes les variables de types ne sont pas differentes dans l'appel de "
     ^ td.dident)
  else
    let rec aux envtyps l =
      match l with
      | [] -> envtyps
      | s :: q -> aux (Smap.add s (Tgeneral s, 0) envtyps) q
    in
    let envvartyps = aux Smap.empty td.identlist in
    let conflit s _ _ = typingerror "conflit dans les patternes" in
    let envtyps = Smap.union conflit envtyps envvartyps in
    let rec addinstance envinstances nlist =
      match nlist with
      | [] -> envinstances
      | n :: q ->
          Smap.add n.nident
            ((List.map (typatype env envtyps envinstances) n.atypes, [])
            :: smapfind n.nident envinstances)
            (addinstance envinstances q)
    in
    let envinstances = addinstance envinstances td.ntypelist in
    print_typ (typpurtyp env envtyps envinstances td.purtype);
    lastdefined := td.dident;
    ( Tarrow
        ( List.map (typpurtyp env envtyps envinstances) td.purtypelist,
          typpurtyp env envtyps envinstances td.purtype ),
      envvartyps,
      envinstances )

and typdfn env envtyps envinstances addtodeflist (df : defn) tlist t =
  if addtodeflist && Smap.mem df.ident !envfonctions && !lastdefined <> df.ident
  then typingerror (df.ident ^ " est définie 2 fois")
  else (
    if addtodeflist then deflist := df :: !deflist;
    let patarglist = df.patargs in
    let conflit s _ _ =
      typingerror ("l'ident " ^ s ^ " est utilisé plusieurs fois")
    in
    let rec aux envi (ptlist : patarg list) tlist =
      match (ptlist, tlist) with
      | [], [] -> envi
      | pt :: q1, t :: q2 ->
          let a = ensuretyppatarg empty envtyps envinstances t pt in
          let env = aux envi q1 q2 in
          print_smap a.bindings;
          {
            bindings = Smap.union conflit a.bindings env.bindings;
            fvars = Vset.union a.fvars env.fvars;
          }
      | _ -> typingerror "Pas le bon nombre d'arguments"
    in
    let env = aux env patarglist tlist in
    let conflit s _ _ = typingerror ("conflit dans les forall avec " ^ s) in
    let envtyps =
      if addtodeflist then
        Smap.union conflit envtyps (sndr (smapfind df.ident !envfonctions))
      else envtyps
    in
    print_string (df.ident ^ "\n");
    List.iter print_typ tlist;
    print_typ t;
    print_int (List.length df.patargs);
    if typexpr env envtyps envinstances addtodeflist df.expr <> t then (
      print_endline "mouais";
      print_typ (typexpr env envtyps envinstances addtodeflist df.expr);
      print_typ t;
      typingerror ("pas le type censé etre renvoyé par " ^ df.ident))
    else ())

and typdata env envtyps envinstances s slist ialist =
  if Smap.mem s !envtyps then
    typingerror ("conflit dans la définition du type " ^ s)
  else if not (alldifferent slist) then
    typingerror
      ("toutes les variables de types ne sont pas differentes dans la \
        définition du type " ^ s)
  else
    let t = Tcustom (s, []) in
    envtyps := Smap.add s (t, List.length slist) !envtyps;
    let rec aux envtyps l =
      match l with
      | [] -> envtyps
      | s :: q -> aux (Smap.add s (Tgeneral s, 0) envtyps) q
    in
    let conflit s _ _ = typingerror ("conflit dans les forall avec " ^ s) in
    let envvartyps = aux Smap.empty slist in
    let envtypsact = Smap.union conflit envvartyps !envtyps in
    let rec aux2 l =
      match l with
      | [] -> ()
      | (i, alist) :: q ->
          if Smap.mem i !envconstructors then
            typingerror ("Le constructeur " ^ i ^ " est défini plusieurs fois")
          else
            envconstructors :=
              Smap.add i
                {
                  cident = i;
                  ctlist = List.map (typatype env envtypsact envinstances) alist;
                  ctyp = t;
                  cenvvartyps = envvartyps;
                }
                !envconstructors;
          aux2 q
    in
    aux2 ialist

and checkforenddef envtyps envinstances =
  let rec isanident p =
    match p with
    | Plident s -> true
    | Ppattern (Ppatarg p) -> isanident p
    | _ -> false
  in
  let rec getchanging dejatrouve i l =
    match l with
    | [] -> -1
    | x :: q when isanident x -> getchanging dejatrouve (i + 1) q
    | x :: q when dejatrouve ->
        typingerror "Dans les fonctions, on filtre au plus une valeur"
    | x :: q ->
        let _ = getchanging true (i + 1) q in
        i
  in
  let rec aux posdufiltrage (df : defn) =
    Ppatarg (List.nth df.patargs posdufiltrage)
  in
  if !lastdefined = "" then ()
  else
    let conflit s _ _ = typingerror ("conflit dans les forall avec " ^ s) in
    let envtyps =
      Smap.union conflit envtyps (sndr (smapfind !lastdefined !envfonctions))
    in
    print_int (List.length !deflist);
    deflist := List.rev !deflist;
    match !deflist with
    | [] ->
        typingerror
          ("La déclaration de " ^ !lastdefined
         ^ " doit être suivie de sa définition")
    | x :: q -> (
        let posdufiltrage = getchanging false 0 x.patargs in
        print_string "On filtre en ";
        print_int posdufiltrage;
        if posdufiltrage = -1 then (
          if List.length !deflist > 1 then
            typingerror (!lastdefined ^ " est définie 2 fois")
          else lastdefined := "";
          deflist := [])
        else
          match fstr (smapfind !lastdefined !envfonctions) with
          | Tarrow (tlist, t) ->
              if
                not
                  (checkexaustivelist empty envtyps envinstances
                     [ List.nth tlist posdufiltrage ]
                     (List.map
                        (fun x -> [ x ])
                        (List.map (aux posdufiltrage) !deflist)))
              then
                typingerror
                  ("Patterne non exhaustif dans la definition de "
                 ^ !lastdefined);
              lastdefined := "";
              deflist := []
          | _ -> failwith "pas possible")

and typclass env envtyps envinstances s slist tdlist =
  if Smap.mem s !envclasses then
    typingerror ("La classe " ^ s ^ " est définie 2 fois");
  let (classenvfonctions : typ Smap.t ref) = ref Smap.empty in
  List.iter
    (fun td ->
      let a, b, d = typtdecl env envtyps envinstances td in
      lastdefined := "";
      deflist := [];
      classenvfonctions := Smap.add td.dident a !classenvfonctions;
      envfonctions := Smap.add td.dident (a, b, Some s, d) !envfonctions)
    (List.map
       (fun { dident; identlist; ntypelist; purtypelist; purtype } ->
         { dident; identlist = slist; ntypelist; purtypelist; purtype })
       tdlist);
  envclasses := Smap.add s (slist, !classenvfonctions, []) !envclasses;
  globalenvinstances := Smap.add s [] !globalenvinstances

and noconseqconflit _ a _ = Some a
and noconseqconflit2 _ a _ = Some a

and ajouter envtyps alist =
  print_endline "ajouter";
  match alist with
  | Aident s :: q when isalident s ->
      print_endline ("ok " ^ s);
      Smap.add s (Tgeneral s, 0) (ajouter envtyps q)
  | Aident _ :: q -> ajouter envtyps q
  | Apurtype p :: q ->
      Smap.union noconseqconflit2 (ajouter envtyps q) (ajouterpurtyp envtyps p)
  | [] -> envtyps

and ajouterpurtyp envtyps p =
  match p with
  | Patype a -> ajouter envtyps [ a ]
  | Pntype n -> ajouterntyp envtyps n

and ajouterntyp envtyps n =
  print_endline "ajoutern";
  ajouter envtyps n.atypes

and ajouterntyplist envtyps nlist =
  print_endline "ajouternl";
  match nlist with
  | [] -> envtyps
  | n :: q ->
      Smap.union noconseqconflit2 (ajouterntyp envtyps n)
        (ajouterntyplist Smap.empty q)

and typinstance env envtyps envinstances i deflist =
  match i with
  | Imularrow (nlist, n) ->
      let clident = n.nident in
      let slist, classenvfonctions, flist = smapfind clident !envclasses in
      let rec addinstance envinstances envtyps nlist =
        match nlist with
        | [] -> envinstances
        | n :: q ->
            Smap.add n.nident
              ((List.map (typatype env envtyps envinstances) n.atypes, [])
              :: smapfind n.nident envinstances)
              (addinstance envinstances envtyps q)
      in
      let envtyps = ajouterntyplist envtyps nlist in
      let envtyps = ajouter envtyps n.atypes in
      let envinstances = addinstance envinstances envtyps nlist in
      let rec aux5 tlist1 tlist2 =
        print_endline "On teste l'unification";
        if unifyable tlist1 tlist2 then
          typingerror
            ("2 instances différentes de " ^ n.nident ^ " peuvent être unifiées")
      in
      print_endline ("ok " ^ n.nident);
      List.iter
        (aux5 (List.map (typatype env envtyps envinstances) n.atypes))
        (List.map fst (smapfind n.nident !globalenvinstances));
      print_endline "pas de probleme ici";
      let rec aux slist tlist =
        match (slist, tlist) with
        | [], [] -> Smap.empty
        | s :: q1, t :: q2 -> Smap.add s t (aux q1 q2)
        | _ -> typingerror "pas le bon nombre de types"
      in
      let substtable =
        aux slist (List.map (typatype env envtyps envinstances) n.atypes)
      in
      let subst t =
        match t with Tgeneral s -> smapfind s substtable | _ -> t
      in
      List.iter
        (fun (df : defn) ->
          match fstr (smapfind df.ident !envfonctions) with
          | Tarrow (tlist, t) ->
              typdfn env envtyps envinstances false df (List.map subst tlist)
                (subst t)
          | _ -> failwith "c'est pas possible")
        deflist;
      let rec isanident p =
        match p with
        | Plident s -> true
        | Ppattern (Ppatarg p) -> isanident p
        | _ -> false
      in
      let rec aux4 s (l : defn list) =
        match l with
        | [] -> []
        | df :: q when df.ident = s -> df :: aux4 s q
        | df :: q -> aux4 s q
      in
      let rec getchanging dejatrouve i l =
        match l with
        | [] -> -1
        | x :: q when isanident x -> getchanging dejatrouve (i + 1) q
        | x :: q when dejatrouve ->
            typingerror "Dans les fonctions, on filtre au plus une valeur"
        | x :: q ->
            let _ = getchanging true (i + 1) q in
            i
      in
      let rec aux posdufiltrage (df : defn) =
        Ppatarg (List.nth df.patargs posdufiltrage)
      in
      let aux3 s t =
        match t with
        | Tarrow (tlist, _) -> (
            let l = aux4 s deflist in
            let () = print_endline "mot clef" in
            let () = print_int (List.length l) in
            match l with
            | [] -> typingerror ("Il manque une définition de " ^ s)
            | x :: q ->
                let posdufiltrage = getchanging false 0 x.patargs in
                if posdufiltrage = -1 then ()
                else if
                  not
                    (checkexaustivelist env envtyps envinstances
                       [ subst (List.nth tlist posdufiltrage) ]
                       (List.map
                          (fun x -> [ x ])
                          (List.map (aux posdufiltrage) l)))
                then
                  typingerror
                    ("pas exhaustif dans l'appel de l'instance pour la classe "
                   ^ clident))
        | _ -> failwith "pas possible"
      in
      let rec aux6 nlist =
        match nlist with
        | [] -> []
        | n :: q ->
            (n.nident, List.map (typatype env envtyps envinstances) n.atypes)
            :: aux6 q
      in
      let listdesoblig = aux6 nlist in
      Smap.iter aux3 classenvfonctions;
      globalenvinstances :=
        Smap.add n.nident
          ((List.map (typatype env envtyps envinstances) n.atypes, listdesoblig)
          :: smapfind n.nident !globalenvinstances)
          !globalenvinstances
  | Iarrow (n1, n2) ->
      typinstance env envtyps envinstances (Imularrow ([ n1 ], n2)) deflist
  | Intype n -> typinstance env envtyps envinstances (Imularrow ([], n)) deflist
