(* multiset implementation from  
 * http://www.lri.fr/~filliatr/ftp/ocaml/misc/anagram.ml.html *)

module MultiSet = Map.Make(struct type t = char let compare = compare end)

let add letter bag =
  try let n = MultiSet.find letter bag in MultiSet.add letter (succ n) bag 
  with Not_found -> MultiSet.add letter 1 bag;;

let remove letter bag =
  let n = MultiSet.find letter bag in 
  if n = 1 then MultiSet.remove letter bag 
  else MultiSet.add letter (pred n) bag;;

(* Remove a letter from a bag
 * if the letter doesn't exist, try removing a blank *)
let play letter bag = 
  try remove letter bag
  with Not_found -> remove '.' bag;;

let of_string str =
  let bag = ref MultiSet.empty in
  String.iter (fun i -> bag := add i !bag) str;
  !bag;;

let empty = MultiSet.empty;;
