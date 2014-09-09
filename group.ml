open Core.Std
include Utility

type t = int list

(* for Set.Elt *)
let compare = List.compare ~cmp:Pervasives.compare
let sexp_of_t = List.sexp_of_t Sexplib.Conv.sexp_of_int
let t_of_sexp = List.t_of_sexp Sexplib.Conv.int_of_sexp

let empty () = []

let sort = List.sort ~cmp:Pervasives.compare

let uniq l = sort (List.dedup l)

let of_char_list l =
  let rec readlist l g =
    match l with
    | [] -> g
    | c :: cs -> match c with
      | 'a' .. 'z' -> readlist cs ((from_lower c) :: g)
      | 'A' .. 'Z' -> readlist cs ((from_upper c) :: g)
      | _ ->  readlist cs g
  in
  uniq (readlist l [])

let of_string s =
  of_char_list (String.to_list_rev s)

let contains s c = List.mem s c
