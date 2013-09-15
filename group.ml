open Utility

module CharSet = Set.Make(Char)

let empty = CharSet.empty

let of_string s =
  let lst = explode s in
  List.fold_left (fun set i -> CharSet.add i set) empty lst

let contains s c = CharSet.mem c s
