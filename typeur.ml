(* Tequa list -> stype *)
let rec recup_guess eq = match eq with
  | (Tequa { tg = tg ; td = td })::t -> 
      if tg = guess then td else
      if td = guess then tg else
      recup_guess t 
  | [] -> Value "Total panic !! , ??? pas trouvé dans les équations."

;;








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
      else Ur { res = resu1.res @ [ Tequa { tg = s ; td = (ctarr ta tr empty_str) }] ; status = "GSUCCES" ; cause = "" }
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


(* >>> a voir comment faire la sytaxe des if séquentiels sans else *)

(* StypeMap[string * Stype -> lambda_terme -> typage_res *)
let typeur_envi envi l =
  if verbeux then Format.printf "\n****** TYPAGE de %s ***\n" (print_lterme l) else () ;
  (* >>> j'ai mis gen_equas_rec parce que gen_equas renvoie eqs.res -> a voir peut etre pk *)
  let Ur eqs = gen_equas_rec envi l guess in
  if eqs.status = "GECHEC" then
    (* Tr { status = "ECHEC" ; res = [] ; cause = eqs.cause } *)
    ("ECHEC", Value "", eqs.cause)
  else
    let Ur ures = unification eqs.res in
    if ures.status = "FINI" then
      let _ = debug ("SUCCES Typage: |- " ^ (print_lterme l) ^ " : " ^ (print_syntax (recup_guess ures.res))) in
      let temp_res = recup_guess ures.res in
      (* Tr { status = "SUCCES" ; res = temp_res ; cause = eqs.cause } *)
      ("SUCCES", temp_res, eqs.cause)
    else
      let _ = debug ("nECHEC Typage: " ^ (print_lterme l) ^ " parce que " ^ ures.cause) in
      (* Tr { status = "ECHEC" ; res = [] ; cause = ures.cause } *)
      ("ECHEC", Value "", ures.cause)
;;

(* lambda_terme -> typage_res *)
let typeur l =
  typeur_envi (StypeMap.empty) l
;;
