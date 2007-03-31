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
 *************************************************************************)

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

let addchar str c =
  let len = String.length str in
  let retval = String.create (len + 1) in
  String.blit str 0 retval 0 len;
  retval.[len] <- c;
  retval;;

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

(*************************************************************************
 * debug
 * ***********************************************************************)

let display_path path = printf "path: {%s %d:%c %s %b} " path.prefix path.node
(letter path.node) (word path) (is_word path);;

let display_list pref lst =
  let rec d l = 
    match l with
    [] -> printf "_";
    | c :: cs -> begin printf "%c::" c; d cs; end
  in
  printf "%s [" pref;
  d lst;
  printf "]"
;;

let display_string_list pref lst =
  let rec d l = 
    match l with
    [] -> printf "_";
    | c :: cs -> begin printf "%s::" c; d cs; end
  in
  printf "%s [" pref;
  d lst;
  printf "]"
;;

let debug pref trail path = 
  display_list pref trail;
  display_path path;
  printf("\n")
;;

(*************************************************************************
 * Actual user-facing utilities
 * ***********************************************************************)

(* check if a word is good *)
let check str =
  let len = (String.length str - 1) in
  let rec walk n i =
    try
      let p = find str.[i] n in
      if i = len then (wordp p) else (walk (ptr p) (i+1))
    with Not_found -> false
  in
  walk start_node 0;;

(* follow a 'trail' of characters or wildcards starting from a given prefix *)
let pattern trail path =
  let retval = ref [] in
  let add_word = function [] -> () | s::_ -> retval := s :: !retval in
  let rec pattern' trail path =
    let try_step tr pth = try pattern' tr (step pth) with Not_found -> () in
    let follow tr pth = 
      match tr with
      |[]    -> add_word (word_of pth);
      |['*'] -> ( add_word (word_of pth); try_step tr pth; )
      |_     -> try_step tr pth    
    in
    match trail with
    |[]        -> ();
    |'.' :: cs -> forstep (follow cs) path
    |'*' :: cs -> ( pattern' cs path; forstep (follow trail) path; )
    |c   :: cs -> try follow cs (sib c path) with Not_found -> ()
  in 
    pattern' trail path;
    List.rev !retval
;;

(* Build all possible words from a bag and a dawg *
 * if all = false, return only words using the entire bag *)
let build bag path all =
  let rec traverse bag path =
    collect_sibs (follow_if bag) path
  and follow_if bag path =
    try 
      let new_bag = Bag.play (letter path.node) bag in
      if new_bag = Bag.empty then word_of path 
      else 
        (if all then (word_of path) else []) @ traverse new_bag (step path)
    with Not_found -> []
 in
 traverse bag path
;;

let anagrams bag path = build bag path false;;

let all_words bag path = build bag path true;;

let _ = 
  List.map (printf "%s\n") 
   (pattern (explode Sys.argv.(1)) start);;
