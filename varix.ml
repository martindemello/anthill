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
include Search

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
