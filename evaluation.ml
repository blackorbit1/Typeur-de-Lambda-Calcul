
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
