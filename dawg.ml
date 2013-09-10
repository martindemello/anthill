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

type dawg = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;;

let load fname =
  let fd = Unix.openfile fname [ Unix.O_RDONLY ] 0 in
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
let lastp dawg ix = _lastp dawg.{ix};;
let wordp dawg ix = _wordp dawg.{ix};;
let letter dawg ix = _letter dawg.{ix};;
let ptr dawg ix = _ptr dawg.{ix};;

(**********************************************************
 * Node functions
 **********************************************************)

(* scan the siblings of a node for a character *)
let rec find dawg chr node =
  let c = letter dawg node in
  if (c == chr) then node
  else if ((c > chr) or (lastp dawg node)) then raise Not_found
  else (find dawg chr (node + 1));;

(**********************************************************
 * Path functions
 **********************************************************)

(* hold a prefix string and a node *)

type path = { prefix: string; node: int };;

let start = { prefix = ""; node = start_node };;

let word dawg p = addchar p.prefix (letter dawg p.node);;

(* take a step forward *)
let step dawg p = match ptr dawg p.node with
  0 -> raise Not_found
| _ -> { prefix = word dawg p; node = ptr dawg p.node };;

(* use uppercase for wildcard matches *)
let uword dawg p u = match u with
| '.' -> addchar p.prefix (Char.uppercase (letter dawg p.node))
| _   -> addchar p.prefix (letter dawg p.node);;

let ustep dawg p u = match ptr dawg p.node with
  0 -> raise Not_found
| _ -> { prefix = uword dawg p u; node = ptr dawg p.node };;

let next_sib dawg p =
  if lastp dawg p.node then raise Not_found
  else { p with node = p.node + 1 };;

let sib dawg c p = { p with node = find dawg c p.node };;

let rec foreach_sib dawg f p =
  f p;
  if not (lastp dawg p.node) then foreach_sib dawg f (next_sib dawg p);;

let is_word dawg p = wordp dawg p.node;;

let word_of dawg path =
  if (is_word dawg path) then Some(word dawg path) else None;;

let uword_of dawg path u =
  if (is_word dawg path) then Some(uword dawg path u) else None;;
