module StringSet = Set.Make(String);;

type t = StringSet.t

let empty = StringSet.empty;;

let of_list lst =
  List.fold_left (fun set i -> StringSet.add i set) empty lst;;

let to_list set =
  StringSet.elements set;;

let to_lower set =
  let add i set = StringSet.add (String.lowercase i) set in
  StringSet.fold add set empty;;
