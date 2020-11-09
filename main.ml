(*  

Projet 1 - TAS

FOUQUET Justin
DUTRA Enzo

*)

#use "types.ml" ;;
#use "utils.ml" ;;


(* === === === Exo 2.1 *)

(* >> voir dans le fichier types.ml *)

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

(* lambda_terme -> remp -> lambda_terme *)
let rec barendregt_rec (lterme : lambda_terme) remp = match lterme with
  | Value v -> (
      try 
        let nvari = (RempMap.find v remp) in cvar nvari
      with 
        Not_found -> cvar v
    )
  | Lambda { vari = v; corps = c } -> 
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

(* string ->  StypeMap[string * Stype] -> StypeMap[string * Stype] -> StypeMap[string * Stype] *)
let envi_fold_func key map acc = (StypeMap.add key (StypeMap.find key map) acc)

(* StypeMap[string * Stype] -> lambda_terme -> syntaxe -> unif_res *)
let rec gen_equas_rec map (l : lambda_terme) s =
  match l with
  | Value v -> (
      try 
        let resu = (StypeMap.find v map) in Ur { res = [(Tequa { tg = resu ; td = s })] ; status = "GSUCCES" ; cause = "" } 
      with 
        Not_found -> Ur { res = [](*Tequa*) ; status = "GECHEC" ; cause = "Pas de" ^ v ^ " dans l'environnement de typage." }
    )
  | Lambda { vari = v ; corps = c } -> 
      let ta = cSvar (fresh_var ()) in
      let tr = cSvar (fresh_var ()) in
      let map2 = StypeMap.add v ta map in (* est ce qu'il faut pas faire un remplacement si y a déjà qqch à cette clé ? *)
      let Ur resu1 = gen_equas_rec map2 c tr in
      if resu1.status = "GECHEC" 
      then Ur resu1
      else Ur { res = Tequa { tg = s ; td = (ctarr ta tr) } :: resu1.res ; status = "GSUCCES" ; cause = "" }
  | Application { fpos = f; apos = a }  -> 
      let ta = cSvar (fresh_var ()) in
      let envi1 = StypeMap.fold envi_fold_func StypeMap.empty map in 
      let envi2 = StypeMap.fold envi_fold_func StypeMap.empty map in
      let Ur resuf = gen_equas_rec envi1 f (ctarr ta s) in
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


(* string -> stype -> bool *)
let rec occur_check v t = match t with
  | Value var -> v = var
  | Lambda { tres = r }  -> occur_check v r
  | Application { targ = a ; tres = r }  -> (occur_check v a) || (occur_check v r)
;;

(* string -> syntaxe -> syntaxe -> syntaxe *)
let rec substitue v ts t = match t with
  | Value var -> if v = var then ts else t
  | Lambda { tres = r } -> ctlist (substitue v ts r)
  | Application { targ = a ; tres = r } -> ctarr (substitue v ts a) (substitue v ts r)
;;

(* string -> stype -> []t_equas -> []t_equas *)
let rec substitue_partout v ts (eqs : t_equas list) = 
  match eqs with
  | [] -> []
  | Tequa eq :: eqs_rest -> 
    let neq = Tequa { tg = (substitue v ts eq.tg) ; td = (substitue v ts eq.td) } in
    neq :: substitue_partout v ts eqs_rest
;;

let guess = ctvar "???" ;;

(* func unification_etape(eqs []tequa, iint) unif_res *)
(* t_equas -> int -> unif_res *)
let unification_etape eqs i =
  if i >= (List.length eqs) then Ur { status = "FINI" ; res = eqs ; cause = "" } else
  if 


;;

(* t_equas -> unif_res *)
let unification eqs = 

;;