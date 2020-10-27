(* 

Projet 1 - TAS

FOUQUET Justin
DUTRA Enzo

*)


(* 
l = "lambda"
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
let cvar str = V str ;;

(* string, l -> l *)
let clam str lambda = L { vari = str ; corps = lambda } ;;

(* l, l -> l *)
let capp lambda1 lambda2 = A { fpos = lambda1 ; apos = lambda2 } ;;

(* Exemples *)
let v1 = cvar "sd" ;;
let v2 = cvar "hy" ;;
let l1 = clam "az" v1 ;;
let a1 = capp l1 v2 ;;

print_lterme v1 ;;
print_lterme l1 ;;
print_lterme a1 ;;


(* === === Visionnage de la video du TD3 de romain === === *)

(* Commentaires de romain pendant le TD 
M ::= x   --> VarOfString
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