(**
*
*  DAWG.H
*
*  Header file for Directed Acyclic Word Graph access
*
*  The format of a DAWG node (8-bit arbitrary data) is:
*
*   31                24 23  22  21                                     0
*  +--------------------+---+---+--+-------------------------------------+
*  |      Letter        | W | N |??|            Node pointer             |
*  +--------------------+---+---+--+-------------------------------------+
*
* where N flags the last edge in a node and W flags an edge as the
* end of a word. 21 bits are used to store the node pointer, so the
* dawg can contain up to 262143 edges. (and ?? is reserved - all code
* generating dawgs should set this bit to 0 for now)
*
* The root node of the dawg is at address 1 (because node 0 is reserved
* for the node with no edges).
*
**)

(*
#load "unix.cma";;
#load "bigarray.cma";;
*)

open Bigarray
open Printf

let dawg = 
  let fd = Unix.openfile "sowpods.dwg" [ Unix.O_RDONLY ] 0 in
  Array1.map_file fd int32 c_layout false (-1);;

(* bitfield accessors *)
let w_pos = Int32.shift_left Int32.one 23;;
let n_pos = Int32.shift_left Int32.one 22;;
let ptr_mask = Int32.of_string "0b0000000000011111111111111111111";;
let start_node = 1;;

let _letter node = Char.chr(Int32.to_int(Int32.shift_right_logical node 24));;
let _wordp node = (Int32.logand node w_pos) <> Int32.zero;;
let _lastp node = (Int32.logand node n_pos) <> Int32.zero;;
let _ptr node = Int32.to_int (Int32.logand node ptr_mask);;

(* access nodes via their dawg index *)
let lastp ix = _lastp dawg.{ix};;
let wordp ix = _wordp dawg.{ix};;
let letter ix = _letter dawg.{ix};;
let ptr ix = _ptr dawg.{ix};;

(**********************************************************
 * String functions 
 **********************************************************)
let explode str =
  let lst = ref [] in
  String.iter (fun c -> lst := (c :: !lst)) str;
  List.rev !lst;;

(**********************************************************
 * Store words as reversed lists for efficiency
 **********************************************************)
let addchar str c =
  let len = String.length str in
  let retval = String.create (len + 1) in
  String.blit str 0 retval 0 len;
  retval.[len] <- c;
  retval;;


(**********************************************************
 * Node functions 
 **********************************************************)

exception No_ptr;;

(* scan the siblings of a node for a character *)
let rec find chr node =
  (*printf "|  %d %c -> %d [%b]\n" node (letter node) (ptr node) (wordp node); *)
  let c = letter node in
  if (c == chr) then node
  else if (c > chr) then raise Not_found 
  else if lastp node then raise Not_found
  else (find chr (node + 1));;

(**********************************************************
 * Path functions 
 **********************************************************)

(* hold a prefix string and a node *)

type path = { prefix: string; mutable node: int };;

let start = { prefix = ""; node = start_node };;

let word p = addchar p.prefix (letter p.node);;

let step p =
  match ptr p.node with
  0 -> raise No_ptr
  |_ -> { prefix = word p; node = ptr p.node };;

let nsib p = 
  { prefix = p.prefix; node = p.node + 1 };;

let inc p = 
  p.node <- p.node + 1;
  p;;

let seek chr p = 
  p.node <- find chr p.node;
  p;;

let sib c p =
  { prefix = p.prefix; node = find c p.node };;

let rec forstep f p =
  f p;
  if not (lastp p.node) then forstep f (nsib p);;

let mapsibs f p =
  let lst = ref [] in
  forstep (fun i -> lst := (f i) :: !lst) p;
  List.rev !lst;;

let isword p = wordp p.node;;

let wordof path = 
  if (isword path) then [word path] else [];;

let check str =
  let s = (String.length str - 1) in
  let rec walk n i =
    let p = find str.[i] n in
    match p with 0 -> false
    |_ -> if i = s then (wordp p) else (walk (ptr p) (i+1))
    in
    walk start_node 0;;


(* follow a 'trail' of characters or wildcards starting from a given prefix *)
let rec build trail path = 
  try
    match trail with
    | [] -> []
    | c :: cs -> match c with
      | '.' -> List.flatten (mapsibs (follow cs) path)
      | _   -> follow cs (seek c path)
  with No_ptr -> [] | Not_found -> []
and
  follow tr pth = match tr with
  [] -> wordof pth
  |_  -> build tr (step pth)
;;

(***********************************************************************
 * anagrams
 * *********************************************************************)

(* multiset implementation from  
 * http://www.lri.fr/~filliatr/ftp/ocaml/misc/anagram.ml.html *)

module Bag = Map.Make(struct type t = char let compare = compare end)

let bag_add letter bag =
  try let n = Bag.find letter bag in Bag.add letter (succ n) bag 
  with Not_found -> Bag.add letter 1 bag;;

let bag_remove letter bag =
  let n = Bag.find letter bag in 
  if n = 1 then Bag.remove letter bag else Bag.add letter (pred n) bag;;

(* Remove a letter from a bag
 * if the letter doesn't exist, try removing a blank *)
let bag_play letter bag = 
  try bag_remove letter bag
  with Not_found -> bag_remove '.' bag;;

let bag_of_string w =
  let n = String.length w in
  let rec add i = if i = n then Bag.empty else bag_add w.[i] (add (succ i)) in
  add 0

let rec anagrams bag path =
  List.flatten (mapsibs (follow_if bag) path)
and follow_if bag path =
  try 
    let new_bag = bag_play (letter path.node) bag in
    if new_bag = Bag.empty then wordof path 
    else anagrams new_bag (step path)
  with Not_found -> [] | No_ptr -> []
;;

(*let _ = 
  List.map (printf "%s\n") 
  (anagrams (bag_of_string "retinas") start);;
*)



let _ = 
  List.map (printf "%s\n") 
  (build (explode "ra.e") start);;
