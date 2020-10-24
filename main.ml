(* 

Projet 1 - TAS

FOUQUET Justin
DUTRA Enzo

*)



type lterme =
    Vari of string
  | Oper of string
  | Val of int
  | Fpos of *lterme
  | Apos of *lterme
  | Corps of *lterme 
(*  | Elements of lterme list *)
;;


(* type prof *)

(*
type I =
    V of string
    | L of { vari : string ; corps : l }
    | A of { fpos : I ; apos : I }
*)

(* === === Visionnage de la video du TD3 de romain === === *)

(* Commentaires de romain pendant le TD *)
M ::= x   --> VarOfString
λx.M      --> AbsOfString * lterme
M M       --> AppOf (lterme) * (lterme)

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