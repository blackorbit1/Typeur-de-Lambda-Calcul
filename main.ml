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

