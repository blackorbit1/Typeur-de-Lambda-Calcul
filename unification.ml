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
  | Application { targ = a ; tres = r } -> ctarr (substitue v ts a) (substitue v ts r) empty_str
;;

(* string -> stype -> []t_equas -> []t_equas *)
let rec substitue_partout v ts (eqs : t_equas list) = 
  match eqs with
  | [] -> []
  | Tequa eq :: eqs_rest -> 
    let neq = Tequa { tg = (substitue v ts eq.tg) ; td = (substitue v ts eq.td) } in
    neq :: substitue_partout v ts eqs_rest
;;

let guess = cSvar "???" ;;

let get_last_poped_to_i l i = 
  let tmp = replace i (get_nth l ((List.length l) - 1)) l in
  sublist 0 ((List.length l) - 2) tmp
;;

(* t_equas list -> int -> unif_res *)
let unification_etape eqs i =
  if i >= (List.length eqs) then Ur { status = "FINI" ; res = eqs ; cause = "" } else
  let Tequa eqs_i = get_nth eqs i in
  if eqs_i.tg = guess then Ur { status = "CONTINUE" ; res = eqs ; cause = "" } else
  if eqs_i.td = guess then Ur { status = "CONTINUE" ; res = eqs ; cause = "" } else
  if stype_egal eqs_i.tg eqs_i.td then 
    let nt = get_last_poped_to_i eqs i in
    Ur { status = "CONTINUE" ; res = nt ; cause = "" }
  else match Tequa eqs_i with
  | Tequa { tg = _ ; td = _ } -> (* sert juste à assurer qu'on a du Tequa *)
    match (eqs_i.tg, eqs_i.td) with
    | (Value vg, Value vd) -> 
      let subv = vg in
      let subt = eqs_i.td in
      let nt = get_last_poped_to_i eqs i in
      Ur { status = "RECOMMENCE" ; res = (substitue_partout subv subt nt) ; cause = "" }
    | (Value v, _) -> 
      if occur_check v eqs_i.td then 
        Ur { status = "ECHEC" ; res = [] ; cause = Printf.sprintf "Variable %s présente dans %s" v (print_syntax eqs_i.td) }
      else
        let subv = v in
        let subt = eqs_i.td in
        let nt = get_last_poped_to_i eqs i in
        Ur { status = "RECOMMENCE" ; res = (substitue_partout subv subt nt) ; cause = "" }
  
    | (_, Value v) -> 
      if occur_check v eqs_i.tg then
        Ur { status = "ECHEC" ; res = [] ; cause = Printf.sprintf "Variable %s présente dans %s" v (print_syntax eqs_i.tg) }
      else
        let subv = v in
        let subt = eqs_i.tg in
        let nt = get_last_poped_to_i eqs i in
        Ur { status = "RECOMMENCE" ; res = (substitue_partout subv subt nt) ; cause = "" }
    
    | (Application { targ = tg_targ ; tres = tg_tres ; tvari = tg_tvari }, Application { targ = td_targ ; tres = td_tres ; tvari = td_tvari }) -> 
      let eq1 = Tequa { tg = tg_targ ; td = td_targ } in
      let eq2 = Tequa { tg = tg_tres ; td = td_tres } in
      let nt = get_last_poped_to_i eqs i in
      Ur { status = "RECOMMENCE" ; res = eq1::eq2::nt ; cause = "" }
    | (Application { tvari = tg_tvari }, _) -> 
      Ur { status = "ECHEC" ; res = [] ; cause = Printf.sprintf "Type fleche %s incompatible avec %s" (print_syntax eqs_i.tg) (print_syntax eqs_i.td) }
    | (Lambda { tres = tg_tres }, Lambda { tres = td_tres }) -> 
      let eq1 = Tequa { tg = tg_tres ; td = td_tres } in
      let nt = get_last_poped_to_i eqs i in
      Ur { status = "RECOMMENCE" ; res = eq1::nt ; cause = "" }
    | (Lambda { tres = tg_tres }, _) -> 
      Ur { status = "ECHEC" ; res = [] ; cause = Printf.sprintf "Type Liste %s incompatible avec %s" (print_syntax eqs_i.tg) (print_syntax eqs_i.td) }
    | (_, _) -> 
      Ur { status = "ECHEC" ; res = [] ; cause = Printf.sprintf "Cas d'Unification non pris en charge. Types à traiter : %s et %s" (print_syntax eqs_i.tg) (print_syntax eqs_i.td) }
    (* est ce qu'on ne devrait pas avoir de "(_, Lambda {tres = tg_tres}) ->" ? *)
;;



(* t_equas list -> int -> int -> unif_res *)
let rec unification_rec eqs i c =
  let Ur resu = unification_etape eqs i in
  let _ = debug ("Indice :" ^ (string_of_int i) ^ "\nEquations:\n" ^ (print_tequas eqs)) in
  if c = max_unif then Ur { status = "EXPIRE" ; res = [] ; cause = "" }
  else match resu.status with
    | "CONTINUE" -> unification_rec resu.res (i + 1) (c + 1)
    | "RECOMMENCE" -> unification_rec resu.res 0 (c + 1)
    | "ECHEC" -> Ur resu
    | "FINI" -> Ur resu
    | _ -> Ur { status = "ECHEC" ; res = [] ; cause = "Problème de nommage de status" }
;;

(* t_equas -> unif_res *)
let unification eqs = 
  let i = 0 in let c = 0 in
  let _ = debug ("Indice :" ^ (string_of_int i) ^ "\nEquations:\n" ^ (print_tequas eqs)) in
  unification_rec eqs i c
;;

