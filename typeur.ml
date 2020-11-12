(* Tequa list -> stype *)
let rec recup_guess eq = match eq with
  | (Tequa { tg = tg ; td = td })::t -> 
      if tg = guess then td else
      if td = guess then tg else
      recup_guess t 
  | [] -> Value "Total panic !! , ??? pas trouvé dans les équations."

;;


(* >>> a voir comment faire la sytaxe des if séquentiels sans else *)

(* StypeMap[string * Stype -> lambda_terme -> typage_res *)
let typeur_envi envi l =
  if verbeux then Format.printf "****** TYPAGE de %s ***" (print_lterme l) else () ;
  (* >>> j'ai mis gen_equas_rec parce que gen_equas renvoie eqs.res -> a voir peut etre pk *)
  let Ur eqs = gen_equas_rec envi l guess in
  if eqs.status = "GECHEC" then
    (* Tr { status = "ECHEC" ; res = [] ; cause = eqs.cause } *)
    ("ECHEC", Value "", eqs.cause)
  else
    let Ur ures = unification eqs.res in
    if ures.status = "FINI" then
      (* if verbeux then Format.printf "SUCCES Typage: |- %s : %s" (print_lterme l) (print_syntax recup_guess ures.res) else () ; *)
      let temp_res = recup_guess ures.res in
      (* Tr { status = "SUCCES" ; res = temp_res ; cause = eqs.cause } *)
      ("SUCCES", temp_res, eqs.cause)
    else
      (* if verbeux then Format.printf "ECHEC Typage: %s parce que %s" (print_lterme l) ures.cause else () ;*)
      (* Tr { status = "ECHEC" ; res = [] ; cause = ures.cause } *)
      ("ECHEC", Value "", ures.cause)
;;

(* lambda_terme -> typage_res *)
let typeur l =
  typeur_envi (StypeMap.empty) l
;;