(* multiset implementation from
 * http://www.lri.fr/~filliatr/ftp/ocaml/misc/anagram.ml.html *)

open Types

module MultiSet = Map.Make(struct type t = node let compare = compare end)

(* note that a bag can contain only one '*' *)
let add letter bag =
  try let n = MultiSet.find letter bag in
  MultiSet.add letter (if letter == Star then n else (succ n)) bag
  with Not_found -> MultiSet.add letter 1 bag;;

let remove letter bag =
  let n = MultiSet.find letter bag in
  if n = 1 then MultiSet.remove letter bag
  else MultiSet.add letter (pred n) bag;;

let remove_blank bag =
  try remove Dot bag
  with Not_found -> let _ = MultiSet.find Star bag in bag;;

(* Remove a letter from a bag
 * if the letter doesn't exist, try removing a blank *)
let play letter bag =
  try (remove letter bag, letter)
  with Not_found -> (remove_blank bag, Dot);;

let of_rack rack =
  let bag = ref MultiSet.empty in
  List.iter (fun i -> bag := add i !bag) rack;
  !bag;;

let empty = MultiSet.empty;;

let is_empty bag =
  try let b = remove Star bag in b == empty
  with Not_found -> bag == empty;;
