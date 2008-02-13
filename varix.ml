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
 * high level string -> [string] interface
 * ***********************************************************************)

let anagrams_of_string str = anagrams (Bag.of_string str) start;;

let patterns_of_string str = pattern (explode str) start;;

let build_of_string str = all_words (Bag.of_string str) start;;

(*************************************************************************
 * main() and friends
 * ***********************************************************************)

type eval = { 
  desc: string; 
  proc: string -> string list;
  sort: string list -> string list;
};;

let anag, patt, rack = 
  { desc = "anagram > "; proc = anagrams_of_string; sort = sort_by caps_in; },
  { desc = "pattern > "; proc = patterns_of_string; sort = sort_by caps_in; },
  { desc = "build > "; proc = build_of_string; sort = sort_by String.length; }

let readline prompt =
  Ledit.set_prompt prompt;
  let buf = Buffer.create 256 in
  let rec loop c = match c with
  | '\n' -> Buffer.contents buf
  | _    -> Buffer.add_char buf c; loop (Ledit.input_char stdin)
  in
  loop (Ledit.input_char stdin);;

let print_instructions = 
  print_endline "Anagram: {letters} or a letters";
  print_endline "Pattern: [letters] or p letters";
  print_endline "Build: b letters";
  print_endline "Use . for a blank and * for any number of blanks";
  print_endline "";
  print_endline "Hit Ctrl-D to exit";
  print_endline "------------------------------------------------";
  flush stdout

let _ = 
  print_instructions;
  let cur = ref anag in
  let stack = Stack.create () in
  try 
    while true do
      let str = readline !cur.desc in
      let input = match str with
      | RE "{" (_* as inp) "}" -> cur := anag; inp
      | RE "[" (_* as inp) "]" -> cur := patt; inp
      | RE ['a' 'A'] space (_* as inp) -> cur := anag; inp
      | RE ['p' 'P'] space (_* as inp) -> cur := patt; inp
      | RE ['b' 'B'] space (_* as inp) -> cur := rack; inp
      | _ -> str
      in
      let result = (!cur.sort (!cur.proc input)) in
      Stack.push result stack;
      List.iter (printf "%s\n") result;
      printf "%d\n" (Stack.length stack);
      flush stdout;
    done
      with End_of_file -> print_newline ();;
