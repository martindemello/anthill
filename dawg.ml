(*pp $PP *)
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

open Bigarray
open Printf
include Utility

let dawg = 
  let fd = Unix.openfile "csw.dwg" [ Unix.O_RDONLY ] 0 in
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
 * Node functions 
 **********************************************************)

(* scan the siblings of a node for a character *)
let rec find chr node =
  let c = letter node in
  if (c == chr) then node
  else if (c > chr) then raise Not_found 
  else if lastp node then raise Not_found
  else (find chr (node + 1));;

(**********************************************************
 * Path functions 
 **********************************************************)

(* hold a prefix string and a node *)

type path = { prefix: string; node: int };;

let start = { prefix = ""; node = start_node };;

let word p = addchar p.prefix (letter p.node);;

let step p =
  match ptr p.node with
  0 -> raise Not_found
  |_ -> { prefix = word p; node = ptr p.node };;

let uword p u = match u with
| '.' -> addchar p.prefix (Char.uppercase (letter p.node))
| _   -> addchar p.prefix (letter p.node);;

let ustep p u =
  match ptr p.node with
  0 -> raise Not_found
  |_ -> { prefix = uword p u; node = ptr p.node };;

let next_sib p =
  if lastp p.node then raise Not_found
  else { prefix = p.prefix; node = p.node + 1 };;

let sib c p =
  { prefix = p.prefix; node = find c p.node };;

let rec forstep f p =
  f p;
  if not (lastp p.node) then forstep f (next_sib p);;

let map_sibs f p =
  let lst = ref [] in
  forstep (fun i -> lst := (f i) :: !lst) p;
  List.rev !lst;;

let collect_sibs f p =
  List.flatten (map_sibs f p);;

let is_word p = wordp p.node;;

let word_of path = 
  if (is_word path) then [word path] else [];;

let uword_of path u = 
  if (is_word path) then [uword path u] else [];;
