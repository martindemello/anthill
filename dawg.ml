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

(*************************************************************************
 * Requires: 
 *   unix.cma
 *   bigarray.cma
 *   str.cma
 *************************************************************************)

open Bigarray
open Printf

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
