(* a list -> int -> 'a *)
let rec get_nth mylist index =
    match mylist with
    | [] -> raise (Failure "empty list")
    | first::rest -> 
        if index = 0 then first 
        else get_nth rest (index-1)
;;