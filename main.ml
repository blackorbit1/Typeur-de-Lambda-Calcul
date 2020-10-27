(* 

Projet 1 - TAS

FOUQUET Justin
DUTRA Enzo

*)


(* === === === Exo 2.1 *)


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
  | L {vari = v; corps = c} -> 
      let nvari = fresh_var () in
      let new_remp = (RempMap.add v nvari remp) in
      clam nvari (barendregt_rec c new_remp)
  | A {fpos = f; apos = a} -> 
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


(* === === === Exo 2.3 *)



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