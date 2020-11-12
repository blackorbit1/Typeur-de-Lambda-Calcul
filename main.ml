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


let var_counter = ref 0 ;;

(* () -> string *)
let fresh_var () = 
  var_counter := !var_counter + 1 ;
  "x" ^ string_of_int(!var_counter) ;;



(* string ->  RempMap[string * string] -> RempMap[string * string] -> RempMap[string * string] *)
let remp_fold_func key map acc = (RempMap.add key (RempMap.find key map) acc) ;;

(* 
               fonc                   acc     map   acc_res
            clé    map   acc res_acc
val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
fold f m a computes (f kN dN ... (f k1 d1 a)...), where k1 ... kN are 
the keys of all bindings in m (in increasing order), and d1 ... dN are 
the associated data.
*)

(* lambda_terme -> remp -> lambda_terme *)
let rec barendregt_rec (lterme : lambda_terme) remp = match lterme with
  | Value v -> (
      try 
        let nvari = (RempMap.find v remp) in cvar nvari
      with 
        Not_found -> cvar v
    )
  | Lambda { vari = v ; corps = c } -> 
      let nvari = fresh_var () in
      let new_remp = (RempMap.add v nvari remp) in
      clam nvari (barendregt_rec c new_remp)
  | Application { fpos = f; apos = a } -> 
      (* TODO : pk c'est pas : let remp1 = RempMap.fold remp_fold_func remp RempMap.empty in *)
      let remp1 = RempMap.fold remp_fold_func RempMap.empty remp in 
      let remp2 = RempMap.fold remp_fold_func RempMap.empty remp in
      capp (barendregt_rec f remp1) (barendregt_rec a remp2)
;;

(* lambda_terme -> lambda_terme *)
let barendregt lterme =
  let remp = RempMap.empty in barendregt_rec lterme remp 
;;

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


(* lambda_terme -> string -> lambda_terme -> lambda_terme *)
let rec instantie (l : lambda_terme) x a = match l with
  | Value v -> if v = x then a else cvar v
  | Lambda { vari = v ; corps = c } -> clam v (instantie c x a)
  | Application { fpos = f ; apos = ap } -> capp (instantie f x a) (instantie ap x a)
;;


let eval = ref 0 ;;

(* lambda_terme -> RempMap[string * string] -> (Content : er) *)
let rec ltrcbv_etape_rec l map = 
  let resu = Content { status = "KO" ; rmem = map ; res = l } in
  match l with
  | Application { fpos = f; apos = a } -> (*158*)
      (match ltrcbv_etape_rec f map with
       | Content { status = "OK" ; res = r } -> (*160*) Content { status = "OK" ; res = (capp r a) ; rmem = map } (*161*)
       | Content { res = r } (*164*) -> 
           (match (f, ltrcbv_etape_rec a map) with (*165*)
            | (_, Content { status = "OK" }) -> (*166*) Content { status = "OK" ; res = (capp f r) ; rmem = map }
            | (Lambda { vari = v ; corps = c }, _) -> Content { status = "OK" ; res = (instantie c v a) ; rmem = map }
            | (Application { fpos = f2 ; apos = a2 }, _) -> (* rien \o/ *) Content { status = "OK" ; res = l ; rmem = map }
            | _ -> resu
           )
      )
  | _ -> resu
;;

(* er -> int -> lambda_terme *)
let rec ltrcbv_etape_loop er eval =
  match (er, eval) with
  | (Content er, 1000) -> Format.printf "%s" "*** STOP : Trop de réductions ***" ; er.res
  | (Content er, _) ->
      let eval = eval + 1 in
      let nouveau = ltrcbv_etape_rec er.res er.rmem in 
      match nouveau with
      | Content { status = "KO" } -> er.res
      | Content { res = r ; rmem = rm } -> Format.printf "→%s" (print_lterme r) ; ltrcbv_etape_loop nouveau eval

(* lambda_terme -> lambda_terme *)
let ltrcbv_etape l = 
  let l_barendregt = (barendregt l) in
  let map = RempMap.empty in
  Format.printf "%s" (print_lterme l_barendregt) ;
  let nouveau = ltrcbv_etape_rec l_barendregt map in 
  match nouveau with
  | Content { status = "KO" } -> l
  | Content { res = r ; rmem = rm } -> Format.printf "→%s" (print_lterme r) ; ltrcbv_etape_loop nouveau 0
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


(* syntaxe -> syntaxe -> bool *)
let rec stype_egal (t1 : syntaxe) (t2 : syntaxe) = match (t1, t2) with
  | (Value v1, Value v2) -> v1 = v2
  | (Lambda { tres = r1 }, Lambda { tres = r2 }) -> stype_egal r1 r2
  | (Application { targ = a1 ; tres = r1 }, Application { targ = a2 ; tres = r2 }) -> (stype_egal a1 a2) && (stype_egal r1 r2)
  | (_, _) -> false
;;

(* === === === Exemples *)

stype_egal t1_app1 t2_app2 ;; (* False *)
stype_egal t1_app1 t1_app1 ;; (* True *)
stype_egal t2_app2 t2_app2 ;; (* True *)

stype_egal t2_var1 t2_var2 ;; (* False *)
stype_egal t2_var1 t2_var1 ;; (* True *)


(* === === === Exo 2.4 *)

let tvar_counter = ref 0 ;;

(* () -> string *)
let fresh_tvar () = 
  tvar_counter := !tvar_counter + 1 ;
  "T" ^ string_of_int(!tvar_counter) 
;;

  

(* string ->  StypeMap[string * Stype] -> StypeMap[string * Stype] -> StypeMap[string * Stype] *)
let envi_fold_func key map acc = (StypeMap.add key (StypeMap.find key map) acc)

(* StypeMap[string * Stype] -> lambda_terme -> syntaxe -> unif_res *)
let rec gen_equas_rec map (l : lambda_terme) s =
  match l with
  | Value v -> (
      try 
        let resu = (StypeMap.find v map) in Ur { res = [(Tequa { tg = resu ; td = s })] ; status = "GSUCCES" ; cause = "" } 
      with 
        Not_found -> Ur { res = [](*Tequa*) ; status = "GECHEC" ; cause = "Pas de " ^ v ^ " dans l'environnement de typage." }
    )
  | Lambda { vari = v ; corps = c } -> 
      let ta = cSvar (fresh_tvar ()) in
      let tr = cSvar (fresh_tvar ()) in
      let _ = debug ((print_syntax ta) ^ " = " ^ v ^ "\n" ^ (print_syntax tr) ^ " = " ^ (print_lterme c)) in
      let map2 = StypeMap.add v ta map in (* est ce qu'il faut pas faire un remplacement si y a déjà qqch à cette clé ? *)
      let Ur resu1 = gen_equas_rec map2 c tr in
      if resu1.status = "GECHEC" 
      then Ur resu1
      else Ur { res = Tequa { tg = s ; td = (ctarr ta tr empty_str) } :: resu1.res ; status = "GSUCCES" ; cause = "" }
  | Application { fpos = f ; apos = a }  -> 
      let ta = cSvar (fresh_tvar ()) in
      let _ = debug ((print_syntax ta) ^ " = " ^ (print_lterme a) ^ "\n" ^ (print_syntax (ctarr ta s empty_str)) ^ " = " ^ (print_lterme f)) in
      let envi1 = StypeMap.fold envi_fold_func StypeMap.empty map in 
      let envi2 = StypeMap.fold envi_fold_func StypeMap.empty map in
      let Ur resuf = gen_equas_rec envi1 f (ctarr ta s empty_str) in
      let Ur resua = gen_equas_rec envi2 a ta in
      if resuf.status = "GECHEC" then Ur resuf else 
      if resua.status = "GECHEC" then Ur resua else 
        let equaf = resuf.res in
        let equaa = resua.res in
        Ur { res = (List.append equaf equaa) ; status = "GSUCCES" ; cause = "" }
;;

(* RempMap[string * Stype] -> lambda_terme -> s -> []Tequa *)
let gen_equas map l s = let Ur ur = gen_equas_rec map l s in ur.res
;;


(* === === === Exo 2.5 *)

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


(* SKK : ((λx.λy.λz.((x z) (y z)) λx.λy.x) λx.λy.x) *)
debug (print_typage_res (typeur ex_skk)) ;;




let liste = get_last_poped_to_i [5;6;7;1;9;4] 3 ;;
debug (String.concat " " (List.map string_of_int liste)) ;;

let nth = get_nth [5;6;7;1;9;4] 2 ;;
debug (string_of_int nth) ;;

Format.printf "\n" ;;