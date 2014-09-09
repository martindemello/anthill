open Core.Std

module S = Set.Make(Group);;

let rec cartesian_product = function
  | [] -> [[]]
  | h :: t ->
    let rest = cartesian_product t in
    List.concat
      (List.map h (fun i -> List.map rest (fun r -> i :: r)))

let product gs =
  let c = cartesian_product gs in
  let c = List.map c Group.sort in
  S.to_list (S.of_list c) 
