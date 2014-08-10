open Core.Std

let empty = []

let of_string s =
  let index c = (Char.to_int c) - 97 in
  let lst = List.map (String.to_list_rev s) index in
  List.sort ~cmp:Pervasives.compare (List.dedup lst)

let contains s c = List.mem s c
