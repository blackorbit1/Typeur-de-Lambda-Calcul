(* === === === lambda terme === === === *)

(* === type *)

type lambda_terme =
  | Value of string
  | Lambda of { vari : string ; corps : lambda_terme }
  | Application of { fpos : lambda_terme ; apos : lambda_terme }
;;

(* === constructeurs *)

(* string -> lambda_terme *)
let cvar str = (Value str : lambda_terme) ;;

(* string, lambda_terme -> lambda_terme *)
let clam str lambda = (Lambda { vari = str ; corps = lambda } : lambda_terme) ;;

(* lambda_terme, lambda_terme -> lambda_terme *)
let capp lambda1 lambda2 = (Application { fpos = lambda1 ; apos = lambda2 } : lambda_terme) ;;


(* === utilitaires *)

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

(* === type *)

type syntaxe =
  | Value of string
  | Lambda of { tres : syntaxe } (* TODO : A changer peut etre *)
  | Application of { targ : syntaxe ; tres : syntaxe ; tvari : string } 
;;

module Stype =
struct (*implem*)
  type t = syntaxe [@@deriving ord]
  let compare a b = 0
end

module StypeMap = Map.Make(String) ;;


(* === constructeurs *)

(* string -> syntaxe *)
let cSvar str = (Value str : syntaxe) ;;

(* syntaxe -> syntaxe *)
let ctlist stype = (Lambda { tres = stype } : syntaxe) ;;

(* syntaxe -> syntaxe -> string -> syntaxe *)
let ctarr t1 t2 str = (Application { targ = t1 ; tres = t2 ; tvari = str } : syntaxe) ;;


(* === utilitaires *)

(* syntaxe -> string *)
let rec print_syntax s = match s with
  | Value v -> v
  | Lambda { tres = t } -> "[" ^ (print_syntax t) ^ "]"
  | Application { targ = a ; tres = r } -> "(" ^ (print_syntax a) ^ ") → " ^ (print_syntax r)
;;

(* syntaxe -> syntaxe -> bool *)
let rec stype_egal (t1 : syntaxe) (t2 : syntaxe) = match (t1, t2) with
  | (Value v1, Value v2) -> v1 = v2
  | (Lambda { tres = r1 }, Lambda { tres = r2 }) -> stype_egal r1 r2
  | (Application { targ = a1 ; tres = r1 }, Application { targ = a2 ; tres = r2 }) -> (stype_egal a1 a2) && (stype_egal r1 r2)
  | (_, _) -> false
;;

(* === === === Génération d'équations === === === *)

type t_equas = Tequa of { tg : syntaxe ; td : syntaxe } ;;

let rec print_tequas eqs = match eqs with 
  | (Tequa h)::t -> (print_syntax h.tg) ^ " = " ^ (print_syntax h.td) ^ "\n" ^ (print_tequas t)
  | [] -> ""

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

(* === === === Résultat du typeur === === === *)

type typage_res =
  (*
  status : SUCCES ou ECHEC
	res    : type trouvé, si SUCCES
  cause  : cause d'échec, si ECHEC
  *)
  (* Tr of {status : string, res : syntaxe; cause : string} *)
  string * syntaxe * string
;;

(* typage_res -> string *)
let print_typage_res (tr : typage_res) = match tr with
  | (status, res,  cause) -> "\nStatus : " ^ status ^ "\nResultat : " ^ (print_syntax res) ^ "\nCause : " ^ cause
  (* | _ -> "typage_res malformé" *)
;;