module StringSet = Set.Make(String);;

let empty = StringSet.empty;;

let of_list lst =
  List.fold_left (fun set i -> 
    StringSet.add (String.lowercase i) set) empty lst;;

let to_list set =
  StringSet.elements set;;
