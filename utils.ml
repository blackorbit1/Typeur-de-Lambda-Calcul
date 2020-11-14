(* https://stackoverflow.com/questions/9795504/return-the-nth-element-of-a-list-in-ocaml *)
(* a list -> int -> 'a *)
let rec get_nth mylist index =
    match mylist with
    | [] -> failwith "get_nth: empty list"
    | first::rest -> 
        if index = 0 then first 
        else get_nth rest (index - 1)
;;

(* https://stackoverflow.com/questions/2710233/how-to-get-a-sub-list-from-a-list-in-ocaml *)
(* int -> int -> a list -> 'a list *)
let rec sublist b e l = 
  match l with
  | [] -> failwith "sublist: empty list"
  | h :: t -> 
    let tail = if e = 0 then [] else sublist (b - 1) (e - 1) t in
    if b > 0 then tail else h :: tail
;;

(* https://stackoverflow.com/questions/37091784/ocaml-function-replace-a-element-in-a-list *)
(* int -> a -> a list -> 'a list *)
let replace pos a l = List.mapi (fun i x -> if i = pos then a else x) l ;;

(* string -> () *)
let debug str = if verbeux then Format.printf "\n%s" str else () ;;