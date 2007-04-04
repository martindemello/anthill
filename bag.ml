(* multiset implementation from  
 * http://www.lri.fr/~filliatr/ftp/ocaml/misc/anagram.ml.html *)

module MultiSet = Map.Make(struct type t = char let compare = compare end)

(* note that a bag can contain only one '*' *)
let add letter bag =
  try let n = MultiSet.find letter bag in 
  MultiSet.add letter (if letter == '*' then n else (succ n)) bag 
  with Not_found -> MultiSet.add letter 1 bag;;

let remove letter bag =
  let n = MultiSet.find letter bag in 
  if n = 1 then MultiSet.remove letter bag
  else MultiSet.add letter (pred n) bag;;

let remove_blank bag =
  try remove '.' bag
  with Not_found -> MultiSet.find '*' bag; bag;;

(* Remove a letter from a bag
 * if the letter doesn't exist, try removing a blank *)
let play letter bag = 
  try remove letter bag
  with Not_found -> remove_blank bag;;

let of_string str =
  let bag = ref MultiSet.empty in
  String.iter (fun i -> bag := add i !bag) str;
  !bag;;

let empty = MultiSet.empty;;

let is_empty bag =
  try let b = remove '*' bag in b == empty
  with Not_found -> bag == empty;;
