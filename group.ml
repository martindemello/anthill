open Core.Std

let from_upper c = (Char.to_int c) - 65

let from_lower c = (Char.to_int c) - 97

let empty = []

let uniq l =
  List.sort ~cmp:Pervasives.compare (List.dedup l)

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
