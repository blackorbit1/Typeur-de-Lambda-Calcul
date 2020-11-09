(* === === === lambda terme === === === *)

(* type *)

type lambda_terme =
  | Value of string
  | Lambda of { vari : string ; corps : lambda_terme }
  | Application of { fpos : lambda_terme ; apos : lambda_terme }
;;

(* constructeurs *)

(* string -> lambda_terme *)
let cvar str = (Value str : lambda_terme) ;;

(* string, lambda_terme -> lambda_terme *)
let clam str lambda = (Lambda { vari = str ; corps = lambda } : lambda_terme) ;;

(* lambda_terme, lambda_terme -> lambda_terme *)
let capp lambda1 lambda2 = (Application { fpos = lambda1 ; apos = lambda2 } : lambda_terme) ;;


(* affichage *)

(* lambda_terme -> string *)
let rec print_lterme lterme = match lterme with
  | Value v -> v
  | Lambda { vari = v; corps = c } -> "λ" ^ v ^ "." ^ (print_lterme c)
  | Application { fpos = f; apos = a } -> "(" ^ (print_lterme f) ^ " " ^ (print_lterme a) ^ ")"
;;

(* === === === contenu / erreur === === === *)

module RempMap = Map.Make(String);;

(* er = "résultat de l'évaluation" *)

type er =
  Content of { 
    status : string ;
    res    : lambda_terme ; 
    rmem   : string RempMap.t
  }
;;


(* === === === Syntaxe === === === *)

(* type *)

type syntaxe =
  | Value of string
  | Lambda of { tres : syntaxe } (* TODO : A changer peut etre *)
  | Application of { targ : syntaxe ; tres : syntaxe } 
;;

module Stype =
struct (*implem*)
  type t = syntaxe [@@deriving ord]
  let compare a b = 0
end

module StypeMap = Map.Make(String) ;;

(* constructeurs *)

(* string -> syntaxe *)
let cSvar str = (Value str : syntaxe) ;;

(* syntaxe, syntaxe -> syntaxe *)
let cSapp stype1 stype2 = (Application { targ = stype1 ; tres = stype2 } : syntaxe) ;;

(* syntaxe -> syntaxe *)
let ctlist stype = (Lambda { tres = stype } : syntaxe) ;;

(* syntaxe -> syntaxe -> syntaxe *)
let ctarr t1 t2 = (Application { targ = t1 ; tres = t2 } : syntaxe) ;;


(* syntaxe -> string *)
let rec print_syntax s = match s with
  | Value v -> v
  | Lambda { tres = t } -> "[" ^ (print_syntax t) ^ "]"
  | Application { targ = a ; tres = r } -> "(" ^ (print_syntax a) ^ ") → " ^ (print_syntax r)
;;

(* === === === Génération d'équations === === === *)

type t_equas = Tequa of { tg : syntaxe ; td : syntaxe } ;;

(* === === === Résultat d'unification === === === *)

type unif_res =
  (* status:
  // "FINI" -> plus rien à faire
  // "CONTINUE" -> on passe à l'équation suivante
  // "RECOMMENCE" -> on a modifie les equations, on recommence
  // "ECHEC" -> Explosion : occur_check ou constructeur incompatibles
  // "GSUCCES" -> succes d'une generation d'equation
  // "GECHEC" -> echec d'une generation d'equation *)
    Ur of { res : t_equas list ; status : string ; cause : string }
;;