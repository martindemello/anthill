(* multiset implementation from
 * http://www.lri.fr/~filliatr/ftp/ocaml/misc/anagram.ml.html *)

open Types

module MultiSet = Map.Make(struct type t = char let compare = Char.compare end)

type bag = {
  letters : int MultiSet.t;
  blanks : int;
  star : bool;
}

let empty = {
  letters =  MultiSet.empty;
  blanks = 0;
  star = false
}

let ms_inc ms letter =
  try
    let n = MultiSet.find letter ms in
    MultiSet.add letter (succ n) ms
  with Not_found ->
    MultiSet.add letter 1 ms

let ms_dec ms letter =
  match MultiSet.find letter ms with
  | 1 -> MultiSet.remove letter ms
  | n -> MultiSet.add letter (pred n) ms;;

let ms_count ms letter =
  try
    MultiSet.find letter ms
  with Not_found ->
    0

let add letter bag = match letter with
| Star -> { bag with star = true }
| Dot -> { bag with blanks = bag.blanks + 1 }
| Letter c -> { bag with letters = (ms_inc bag.letters c) }
| Group _ -> raise Unsupported_feature

let has_letter bag letter = match letter with
| Star -> bag.star
| Dot -> bag.blanks > 0
| Letter c -> ms_count bag.letters c > 0
| Group _ -> raise Unsupported_feature

let remove bag letter =
  if has_letter bag letter then
    let new_bag = match letter with
    | Star -> { bag with star = false }
    | Dot -> { bag with blanks = bag.blanks - 1}
    | Letter c -> { bag with letters = ms_dec bag.letters c }
    | Group _ -> raise Unsupported_feature
    in (new_bag, Some letter)
  else
    (bag, None)

(* Remove a letter from a bag
 * if the letter doesn't exist, try removing a blank *)
let play bag letter =
  match remove bag letter with
  | (_, Some c) as b -> b
  | _, None ->
      if bag.star then
        (bag, Some letter)
      else
        remove bag Dot

let of_rack rack =
  let bag = ref empty in
  List.iter (fun i -> bag := add i !bag) rack;
  !bag;;

let is_empty bag =
  bag.blanks == 0 &&
  MultiSet.is_empty bag.letters

let has_wildcards bag =
  bag.blanks == 0 && not bag.star

let iter_letters bag f =
  MultiSet.iter (fun c _ -> f c) bag.letters
