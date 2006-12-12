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
  (*printf "|  %d %c -> %d [%b]\n" node (letter node) (ptr node) (wordp node); *)
  let c = letter node in
  if (c == chr) then node
  else if (c > chr) then 0
  else if lastp node then 0
  else (find chr (node + 1));;

(**********************************************************
 * Path functions 
 **********************************************************)

(* hold a prefix string and a node *)

type path = { prefix: string; mutable node: int };;

let start = { prefix = ""; node = start_node };;

let word p = addchar p.prefix (letter p.node);;

let step p = 
  { prefix = word p; node = ptr p.node };;

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

let mapstep f p =
  let lst = ref [] in
  forstep (fun i -> lst := (f i) :: !lst) p;
  List.rev !lst;;

let isword p = wordp p.node;;

let check str =
  let s = (String.length str - 1) in
  let rec walk n i =
    let p = find str.[i] n in
    match p with 0 -> false
    |_ -> if i = s then (wordp p) else (walk (ptr p) (i+1))
    in
    walk start_node 0;;


(* given a path, find all path+1 words *)
let allwords path = 
  let lst = ref [] in
  forstep
  (fun i -> if (isword i) then lst := (word i) :: !lst)
  path;
  List.rev !lst;;

let terminator c path = 
  match c with
  | '.' -> allwords path
  | _   -> if (isword (seek c path)) then [word path] else [];;

(* follow a 'trail' of characters or wildcards starting from a given prefix *)
let rec build trail path = 
  let rest tr pth = build tr (step pth) in
  if path.node = 0 then [] else
    match trail with
    | [] -> []
    | [c] -> terminator c path
    | c :: cs -> begin
      match c with
      | '.' -> List.flatten (mapstep (rest cs) path)
      | _   -> rest cs (seek c path)
      end;;

(***********************************************************************
 * anagrams
 * *********************************************************************)

(* multiset implementation from  
 * http://www.lri.fr/~filliatr/ftp/ocaml/misc/anagram.ml.html *)

module Cmap = Map.Make(struct type t = char let compare = compare end)

let ms_add c m =
  try let n = Cmap.find c m in Cmap.add c (succ n) m
  with Not_found -> Cmap.add c 1 m

let ms_remove c m =
  let n = Cmap.find c m in 
  if n = 1 then Cmap.remove c m else Cmap.add c (pred n) m

let ms_of_string w =
  let n = String.length w in
  let rec add i = if i = n then Cmap.empty else ms_add w.[i] (add (succ i)) in
  add 0

let wordof path = 
  if (isword path) then [word path] else [];;

let rec anagrams bag path =
  if path.node = 0 then [] 
  else 
    begin
      let l = ref [] in
      let augment new_bag node = 
        l := if new_bag = Cmap.empty 
        then (wordof node) @ !l
        else (anagrams (new_bag) (step (node))) @ !l;
        in
        Cmap.iter (fun c _ -> 
          let new_bag = (ms_remove c bag) in
          match c with
          | '.' -> forstep (fun char_node -> augment new_bag char_node) path;
          |  _  -> 
            begin
              let char_node = (sib c path) in
              match (char_node.node) with
              | 0 -> (); 
              | _ -> augment new_bag char_node
              end
        ) bag;
        !l;
            end;;

let _ = 
  List.map (printf "%s\n") 
  (List.rev (anagrams (ms_of_string "pot.") start));;

(*
let _ = 
  List.map (printf "%s\n") 
  (build (explode "r.....s") start);;*)
