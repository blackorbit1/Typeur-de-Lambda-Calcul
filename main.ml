(*  

Projet 1 - TAS

FOUQUET Justin
DUTRA Enzo

*)

(* Variables globales *)
let max_unif = 1000 ;;
let verbeux = true ;;

(* Imports *)
#use "types.ml" ;;
#use "utils.ml" ;;


(* === === === Exo 2.1 *)

(* >> voir dans le fichier types.ml *)

(* === === === Exemples *)

let empty_str = "" ;;

let v1 = cvar "sd" ;;
let v2 = cvar "hy" ;;
let l1 = clam "az" v1 ;;
let a1 = capp l1 v2 ;;

(* Affichages : *)

print_lterme v1 ;;
print_lterme l1 ;;
print_lterme a1 ;;


(* === === === Exo 2.2 *)

# use "evaluation.ml" ;;

(* === === === Exemples Barendregt *)

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


(* === === === Exemple Left-to-Right Call-by-Value *)

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

(* >> Voir dans le fichier types.ml *)

(* === === === Exemples *)

(*
Test 1 : (A) → (B) → C
*)

let t1_var1 = cSvar "A" ;;
let t1_var2 = cSvar "B" ;;
let t1_var3 = cSvar "C" ;;

let t1_app2 = ctarr t1_var2 t1_var3 empty_str ;;
let t1_app1 = ctarr t1_var1 t1_app2 empty_str ;;

print_syntax t1_app1 ;;

(*
Test 2 : ((A) -> B) -> C
*)

let t2_var1 = cSvar "A" ;;
let t2_var2 = cSvar "B" ;;
let t2_var3 = cSvar "C" ;;

let t2_app1 = ctarr t2_var1 t2_var2 empty_str ;;
let t2_app2 = ctarr t2_app1 t2_var3 empty_str ;;

print_syntax t2_app2 ;;


(* Voir stype_egal dans types.ml *)

(* === === === Exemples *)

stype_egal t1_app1 t2_app2 ;; (* False *)
stype_egal t1_app1 t1_app1 ;; (* True *)
stype_egal t2_app2 t2_app2 ;; (* True *)

stype_egal t2_var1 t2_var2 ;; (* False *)
stype_egal t2_var1 t2_var1 ;; (* True *)


(* === === === Exo 2.4 *)

(* Voir toutes les fonctions dans typeur.ml *)


(* === === === Exo 2.5 *)

let guess = cSvar "???" ;;

#use "unification.ml" ;;
(* occur_check :        string -> stype -> bool *)
(* substitue :          string -> syntaxe -> syntaxe -> syntaxe *)
(* substitue_partout :  string -> stype -> []t_equas -> []t_equas *)


#use "typeur.ml" ;;
(* recup_guess :        Tequa list -> stype *)
(* typeur_envi :        StypeMap[string * Stype -> lambda_terme -> typage_res *)
(* typeur :             lambda_terme -> typage_res *)
(* unification_etape :  t_equas list -> int -> unif_res *)
(* unification_rec :    t_equas -> int -> int -> unif_res *)
(* unification :        t_equas -> unif_res *)


(* === === === Exemples *)

(* (x y) *)
(* debug (print_typage_res (typeur a1)) ;; *)

(* (λx.(x y)) (λx.x) *)
(* debug (print_typage_res (typeur a2)) ;; *)

(* (λy.(λx.(x y))) (λx.x) *)
let a3 = clam "y" a2 ;;
(* debug (print_typage_res (typeur a3)) ;; *)


(* I *)
let ex_id = clam "x" (cvar "x") ;;

(* K *)
let ex_k = clam "x" (clam "y" (cvar "x")) ;;

(* S *)
let ex_s = clam "x" (clam "y" (clam "z" (capp (capp (cvar "x") (cvar "z")) (capp (cvar "y") (cvar "z"))))) ;;

(* SKK *)
let ex_skk = capp (capp ex_s ex_k) ex_k ;;

(* d *)
let ex_delta = clam "x" (capp (cvar "x") (cvar "x")) ;;

(* OM *)
let ex_om = capp ex_delta ex_delta ;;

(* KII *)
let ex_kii = capp (capp ex_k ex_id) ex_id ;;

(* triple *)
let ex_triple = clam "x" (capp (capp (cvar "x") (cvar "x")) (cvar "x")) ;;

(* KSK *)
let ex_kii = capp (capp ex_k ex_s) ex_k ;;


(* SKK : ((λx.λy.λz.((x z) (y z)) λx.λy.x) λx.λy.x) *)
(* debug (print_typage_res (typeur ex_skk)) ;; *)

(* OM : (λx.(x x) λx.(x x)) *)
(* debug (print_typage_res (typeur ex_om)) ;; *)

(* KII : ((λx.λy.x λx.x) λx.x) *)
(* debug (print_typage_res (typeur ex_kii)) ;; *)

(* triple : (λx.(x x) λx.(x x)) *)
(* debug (print_typage_res (typeur ex_triple)) ;; *)

(* KSK : ((λx.λy.x λx.λy.λz.((x z) (y z))) λx.λy.x) *)
(* debug (print_typage_res (typeur ex_kii)) ;; *)





Format.printf "\n" ;;