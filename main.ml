(* 

Projet 1 - TAS

FOUQUET Justin
DUTRA Enzo

*)


(* === === === Exo 2.1 *)


(* 
l = "lambda terme"
V = value
L = lambda
A = application 
*)
type l =
      V of string
    | L of { vari : string ; corps : l }
    | A of { fpos : l ; apos : l }
;;

(* l -> string *)
let rec print_lterme lterme = match lterme with
  | V v -> v
  | L {vari = v; corps = c} -> "λ" ^ v ^ "." ^ (print_lterme c)
  | A {fpos = f; apos = a} -> "(" ^ (print_lterme f) ^ " " ^ (print_lterme a) ^ ")"
;;
  
(* string -> l *)
let cvar str = (V str : l) ;;

(* string, l -> l *)
let clam str lambda = (L { vari = str ; corps = lambda } : l) ;;

(* l, l -> l *)
let capp lambda1 lambda2 = (A { fpos = lambda1 ; apos = lambda2 } : l) ;;

(* === === === Exemples *)

let v1 = cvar "sd" ;;
let v2 = cvar "hy" ;;
let l1 = clam "az" v1 ;;
let a1 = capp l1 v2 ;;

(* Affichages : *)

print_lterme v1 ;;
print_lterme l1 ;;
print_lterme a1 ;;


(* === === === Exo 2.2 *)


module RempMap = Map.Make(String);;
let var_counter = ref 0 ;;

(* () -> string *)
let fresh_var () = 
  var_counter := !var_counter + 1 ;
  "x" ^ string_of_int(!var_counter) ;;


(* string ->  RempMap[string * string] -> RempMap[string * string] -> RempMap[string * string] *)
let remp_fold_func key map acc = (RempMap.add key (RempMap.find key map) acc)

(*        en fait c'est  ---->        acc     map   acc_res
               fonc                   map     acc   acc_res
            clé    map   acc res_acc
val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
fold f m a computes (f kN dN ... (f k1 d1 a)...), where k1 ... kN are 
the keys of all bindings in m (in increasing order), and d1 ... dN are 
the associated data.
*)

(* l -> remp -> l *)
let rec barendregt_rec lterme remp = match lterme with
  | V v -> (
      try 
        let nvari = (RempMap.find v remp) in cvar nvari
      with 
        Not_found -> cvar v
    )
  | L { vari = v; corps = c } -> 
      let nvari = fresh_var () in
      let new_remp = (RempMap.add v nvari remp) in
      clam nvari (barendregt_rec c new_remp)
  | A { fpos = f; apos = a } -> 
      (* TODO : pk c'est pas : let remp1 = RempMap.fold remp_fold_func remp RempMap.empty in *)
      let remp1 = RempMap.fold remp_fold_func RempMap.empty remp in 
      let remp2 = RempMap.fold remp_fold_func RempMap.empty remp in
      capp (barendregt_rec f remp1) (barendregt_rec a remp2)
;;

(* l -> l *)
let barendregt lterme =
  let remp = RempMap.empty in barendregt_rec lterme remp ;;

(* === === === Exemples *)

(* On construit l'expression suivante : (λx.xy) (λx.x) *)
(* λx.(x y) *)
let x1 = cvar "x" ;;
let y1 = cvar "y" ;;
let a1 = capp x1 y1 ;;
let l1 = clam "x" a1 ;;
(* λx.x *)
let x2 = cvar "x" ;;
let l2 = clam "x" x2 ;;
(* (λx.(x y)) (λx.x) *)
let a2 = capp l1 l2 ;;

(* Affichages : *)
a2 ;;
print_lterme a2 ;;
print_lterme (barendregt a2) ;;


(* l -> string -> l -> l *)
let rec instantie l x a = match l with
  | V v -> if v = x then a else cvar v
  | L { vari = v ; corps = c } -> clam v (instantie c x a)
  | A { fpos = f ; apos = ap } -> capp (instantie f x a) (instantie ap x a)
;;


(* 
er = "résultat de l'évaluation"
*)
type er = C of { 
  status : string ;
  res    : l ; 
  rmem   : string RempMap.t
} ;;

let eval = ref 0 ;;

(* l -> RempMap[string * string] -> (C : er) *)
let rec ltrcbv_etape_rec l map = 
  let resu = C { status = "KO" ; rmem = map ; res = l } in 
  match l with
    | A { fpos = f; apos = a } -> (*158*)
      (match ltrcbv_etape_rec f map with
        | C { status = "OK" ; res = r } -> (*160*) C { status = "OK" ; res = (capp r a) ; rmem = map } (*161*)
        | C { res = r } (*164*) -> 
          (match (f, ltrcbv_etape_rec a map) with (*165*)
            | (_, C { status = "OK" }) -> (*166*) C { status = "OK" ; res = (capp f r) ; rmem = map }
            | (L { vari = v ; corps = c }, _) -> C { status = "OK" ; res = (instantie c v a) ; rmem = map }
            | (A { fpos = f2 ; apos = a2 }, _) -> (* rien \o/ *) C { status = "OK" ; res = l ; rmem = map }
            | _ -> resu
          )
      )
    | _ -> resu
;;

(* er -> int -> l *)
let rec ltrcbv_etape_loop er eval =
  match (er, eval) with
    | (C er, 1000) -> Format.printf "%s" "*** STOP : Trop de réductions ***" ; er.res
    | (C er, _) ->
      let eval = eval + 1 in
      let nouveau = ltrcbv_etape_rec er.res er.rmem in 
      match nouveau with
        | C { status = "KO" } -> er.res
        | C { res = r ; rmem = rm } -> Format.printf "→%s" (print_lterme r) ; ltrcbv_etape_loop nouveau eval

(* l -> l *)
let ltrcbv_etape l = 
  let l_barendregt = (barendregt l) in
  let map = RempMap.empty in
  Format.printf "%s" (print_lterme l_barendregt) ;
  let nouveau = ltrcbv_etape_rec l_barendregt map in 
  match nouveau with
      | C { status = "KO" } -> l
      | C { res = r ; rmem = rm } -> Format.printf "→%s" (print_lterme r) ; ltrcbv_etape_loop nouveau 0
;;


(* === === === Exemple *)

(* a2 : (λx.(x y)) (λx.x) *)
ltrcbv_etape a2 ;;

(*(λx.xx)*)
let ox1 = cvar "x" ;;
let oa1 = capp ox1 ox1 ;;
let ol1 = clam "x" oa1 ;;

(*(λx.xx)*)
let ox2 = cvar "x" ;;
let oa2 = capp ox2 ox2 ;;
let ol2 = clam "x" oa1 ;;

(* (λx.xx) (λx.xx) *)
let omega = capp ol1 ol2 ;;

(* ltrcbv_etape omega ;;*)



(* === === === Exo 2.3 *)

(* 
s = syntaxe
V = variable
L = arguments
A = resultat 
*)
type s =
  V of string
| A of { targ : s ; tres : s } 
;;


(* string -> s *)
let cSvar str = (V str : s) ;;

(* s, s -> s *)
let cSapp stype1 stype2 = (A { targ = stype1 ; tres = stype2 } : s) ;;

(* s -> string *)
let rec print_syntax s = match s with
  | V v -> v
  | A { targ = a ; tres = r } -> "(" ^ (print_syntax a) ^ ") → " ^ (print_syntax r)
;;

(* === === === Exemples *)

(*
Test 1 : (A) → (B) → C
*)

let t1_var1 = cSvar "A" ;;
let t1_var2 = cSvar "B" ;;
let t1_var3 = cSvar "C" ;;

let t1_app2 = cSapp t1_var2 t1_var3 ;;
let t1_app1 = cSapp t1_var1 t1_app2 ;;

print_syntax t1_app1 ;;

(*
Test 2 : ((A) -> B) -> C
*)

let t2_var1 = cSvar "A" ;;
let t2_var2 = cSvar "B" ;;
let t2_var3 = cSvar "C" ;;

let t2_app1 = cSapp t2_var1 t2_var2 ;;
let t2_app2 = cSapp t2_app1 t2_var3 ;;

print_syntax t2_app2 ;;


(* s -> s -> bool *)
let rec stype_egal t1 t2 = match (t1, t2) with
  | (V v, A a) -> false
  | (A a, V v) -> false
  | (V v1, V v2) -> v1 = v2
  | (A { targ = a1 ; tres = r1 }, A { targ = a2 ; tres = r2 }) -> (stype_egal a1 a2) && (stype_egal r1 r2)
;;

(* === === === Exemples *)

stype_egal t1_app1 t2_app2 ;; (* False *)
stype_egal t1_app1 t1_app1 ;; (* True *)
stype_egal t2_app2 t2_app2 ;; (* True *)

stype_egal t2_var1 t2_var2 ;; (* False *)
stype_egal t2_var1 t2_var1 ;; (* True *)


(* === === === Exo 2.4 *)

module Stype =
  struct (*implem*)
    type t = s [@@deriving ord]
    let compare a b = 0
  end


module StypeMap = Map.Make(String) ;;

type t = Tequa of { tg : string ; td : string } ;;

type unif_res =
  (* status:
  // "FINI" -> plus rien à faire
	// "CONTINUE" -> on passe à l'équation suivante
	// "RECOMMENCE" -> on a modifie les equations, on recommence
	// "ECHEC" -> Explosion : occur_check ou constructeur incompatibles
	// "GSUCCES" -> succes d'une generation d'equation
	// "GECHEC" -> echec d'une generation d'equation *)
  Ur of { res : t list ; status : string ; cause : string }
;;

(* s -> s -> s *)
let ctarr t1 t2 = A { targ = t1 ; tres = t2 } ;;


(* string ->  StypeMap[string * stype] -> StypeMap[string * stype] -> StypeMap[string * stype] *)
let stype_fold_func key map acc = (StypeMap.add key (StypeMap.find key map) acc)



(* StypeMap[string * stype] -> l -> s -> unif_res *)
let rec gen_equas_rec map l s =
  match (l : l) with
    | V v -> (
      try 
        let res = (StypeMap.find v map) in Ur { res = [(Tequa { tg = res ; td = s })] ; status = "GSUCCES" ; cause = "" }
      with 
        Not_found -> Ur { res = [](*Tequa*) ; status = "GECHEC" ; cause = "Pas de" ^ v ^ " dans l'environnement de typage." }
      )
    | L { vari = v ; corps = c } -> 
      let ta = cSvar (fresh_var ()) in
      let tr = cSvar (fresh_var ()) in
      let map = StypeMap.add v ta map in (* est ce qu'il faut pas faire un remplacement si y a déjà qqch à cette clé ? *)
      let resu1 = gen_equas_rec map c tr in
      if resu1.status = "GECHEC" 
        then resu1
        else Ur { res = Tequa { tg = s, td = (ctarr ta tr) } :: resu1.res ; status = "GSUCCES" ; cause = "" }
    | A { fpos = f; apos = a }  -> 
      let ta = cSvar (fresh_var ()) in
      let envi1 = StypeMap.fold envi_fold_func StypeMap.empty map in 
      let envi2 = StypeMap.fold envi_fold_func StypeMap.empty map in
      let resuf = gen_equas_rec envi1 f (ctarr ta s) in
      let resua = gen_equas_rec envi2 a ta in
      if resuf.status = "GECHEC" then resuf else 
      if resua.status = "GECHEC" then resua else 
      let equaf = resuf.res in
      let equaa = resua.res in
      Ur { res = (List.append equaf equaa) ; status = "GSUCCES" ; cause = "" }
;;



(* func gen_equas(envi map[string]stype, l lterme, t stype) []Tequa *)
(* RempMap[string * stype] -> l -> s -> []Tequa *)
let gen_equas map l s = let ur = gen_equas_rec map l s in ur.res
;;



(* === === Visionnage de la video du TD3 de romain === === *)

(*
λx.M      --> AbsOfString * lterme
M M       --> AppOf (lterme) * (lterme)
*)
 
(* 
Pour éviter de faire "qqch de syntaxique" cad changer
les noms de variables à la voler pour pas que 2 variables
différentes aient le meme nom, on peut regarder les indices 
de Debruijn 
*)

(*
partie Sémentique (optionnelle), pour évaluer, on fait de la 
gauche vers la droite (voir à 34:30 sur la video du TD)
*)

(*
Parties demandées : 1 à 4
paris 5 pas obligatoire
*)