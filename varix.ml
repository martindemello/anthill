(*pp $PP *)

(*************************************************************************
 * Requires: 
 *   unix.cma
 *   bigarray.cma
 *   str.cma
 *************************************************************************)

open Printf
open Debug
include Dawg

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
 * if all = false, return only words using the entire bag 
 *
 * TODO: same 'too-generous exception' fix that pattern needed
 * *)
let build bag path all =
  let retval = ref [] in
  let add_word = function [] -> () | s::_ -> retval := s :: !retval in
  let rec traverse bag path =
    forstep (follow_if bag) path
  and follow_if bag path =
    try 
      let new_bag, played = Bag.play (letter path.node) bag in
      if Bag.is_empty new_bag then add_word (uword_of path played)
      else 
        (if all then add_word (uword_of path played) else ());
        traverse new_bag (ustep path played)
    with Not_found -> ()
 in
 traverse bag path;
 List.rev !retval
;;

let anagrams bag path = build bag path false;;

let all_words bag path = build bag path true;;

(*************************************************************************
 * list transformation and display
 * ***********************************************************************)

let caps_in str =
  let lst = ref [] in
  String.iter 
  (fun c -> if ((c <= 'Z') && (c >= 'A')) then lst := (c :: !lst) else ()) 
  str;
  List.sort compare (List.rev !lst);;

let sort_by f l = 
  List.map snd (List.sort compare (List.map (fun x -> f x, x) l));;


(*************************************************************************
 * high level string -> [string] interface
 * ***********************************************************************)

let anagrams_of_string str = anagrams (Bag.of_string str) start;;

let patterns_of_string str = pattern (explode str) start;;

(*************************************************************************
 * main() and friends
 * ***********************************************************************)

type eval = { desc: string; proc: string -> string list };;

let anag = { desc = "anagram > "; proc = anagrams_of_string };;
let patt = { desc = "pattern > "; proc = patterns_of_string };;

let readline prompt =
  Ledit.set_prompt prompt;
  let buf = Buffer.create 256 in
  let rec loop c = match c with
  | '\n' -> Buffer.contents buf
  | _    -> Buffer.add_char buf c; loop (Ledit.input_char stdin)
  in
  loop (Ledit.input_char stdin);;

let _ = 
  print_endline "Anagram: {letters} or a letters";
  print_endline "Pattern: [letters] or p letters";
  print_endline "Use . for a blank and * for any number of blanks";
  print_endline "";
  print_endline "Hit Ctrl-D to exit";
  print_endline "------------------------------------------------";
  flush stdout;
  let cur = ref anag in
  try 
    while true do
      let str = readline !cur.desc in
      let input = match str with
      | RE "{" (_* as inp) "}" -> cur := anag; inp
      | RE "[" (_* as inp) "]" -> cur := patt; inp
      | RE ['a' 'A'] space (_* as inp) -> cur := anag; inp
      | RE ['p' 'P'] space (_* as inp) -> cur := patt; inp
      | _ -> str
      in
      List.iter (printf "%s\n") (sort_by caps_in (!cur.proc input));
      flush stdout;
    done
      with End_of_file -> print_newline ();;
