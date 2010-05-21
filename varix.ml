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
include Sset

(*************************************************************************
 * high level string -> [string] interface
 * ***********************************************************************)

let anagrams_of_string str = anagrams (Bag.of_string str) start;;

let patterns_of_string str = pattern (explode str) start;;

let build_of_string str = all_words (Bag.of_string str) start;;

(*************************************************************************
 * Cstack 
 * ***********************************************************************)
type eval = { 
  desc: string; 
  proc: string -> string list;
  sort: string list -> string list;
};;

type op = Union | Inter | Diff ;;

type elem = Nop | Primitive of eval * string | Op of op | Words of string list 

let list_of_elem e =
  match e with
  | Words li -> li
  | _ -> failwith "not a wordlist"

let set_of_elem e = 
  Sset.of_list (List.map String.lowercase (list_of_elem e))

let top3 stack = 
  let a = Stack.pop stack in
  let b = Stack.pop stack in
  let c = Stack.pop stack in
  (a, b, c)

let set_op sop stack = 
  let _, l1, l2 = top3 stack in
  let s1, s2 = set_of_elem l1, set_of_elem l2 in
  let s = sop s1 s2 in
  let words = Words (to_list s) in
  Stack.push words stack; stack

let union stack =
  set_op StringSet.union stack

let inter stack =
  set_op StringSet.inter stack

let diff stack =
  set_op StringSet.diff stack

let run stack =
  match Stack.pop stack with
  | Primitive (e, i) ->
      let result = (e.sort (e.proc i)) in
      Stack.push (Words result) stack; stack
  | _ -> failwith "called run with nonprimitive"

let display_top stack =
  let top = Stack.top stack in 
  match top with
  | Words ws        -> List.iter (printf "%s\n") ws
  | Primitive (e,i) -> printf "(%s %s)\n" e.desc i
  | Op Union        -> printf "-or-\n"
  | Op Inter        -> printf "-and-\n"
  | Op Diff         -> printf "-diff-\n"
  | Nop             -> printf "[]\n"

let process stack = 
  let top = Stack.top stack in 
  match top with
  | Words _         -> stack
  | Primitive (_,_) -> run stack
  | Op Union        -> union stack
  | Op Inter        -> inter stack
  | Op Diff         -> diff  stack
  | Nop             -> Stack.pop stack; stack
      
(*************************************************************************
 * main() and friends
 * ***********************************************************************)

let anag, patt, rack = 
  { desc = "anagram > "; proc = anagrams_of_string; sort = sort_by caps_in; },
  { desc = "pattern > "; proc = patterns_of_string; sort = sort_by caps_in; },
  { desc = "build > "; proc = build_of_string; sort = sort_by String.length; }

let readline prompt =
  Ledit.set_prompt prompt;
  let buf = Buffer.create 256 in
  let rec loop c = match c with
  | "\n" -> Buffer.contents buf
  | _    -> Buffer.add_string buf c; loop (Ledit.input_char stdin)
  in
  loop (Ledit.input_char stdin);;

let print_instructions = 
  print_endline "Anagram: a letters";
  print_endline "Pattern: p letters";
  print_endline "Build: b letters";
  print_endline "Use . for a blank and * for any number of blanks";
  print_endline "";
  print_endline "Hit Ctrl-D to exit";
  print_endline "------------------------------------------------";
  flush stdout

let eval_of op = 
  match (String.lowercase op) with
  | "a" -> anag
  | "p" -> patt
  | "b" -> rack
  | _ -> failwith "no such operation!"

(* ---------------------------------------------------
 * Regular expressions
 * --------------------------------------------------- *)

RE tile = alpha | ['.' '*']
RE rack = tile*
RE unary = ['A' 'a' 'P' 'p' 'B' 'b']

let _ = 
  print_instructions;
  let cur = ref anag in
  let stack = Stack.create () in
  try 
    while true do
      try
        let str = readline !cur.desc in
        let thunk = match str with
        | RE (unary as op) space (rack as inp) -> 
            cur := eval_of op; Primitive(!cur, inp)
        | RE bol "and" eol  -> Op Inter
        | RE bol "or" eol   -> Op Union
        | RE bol "diff" eol -> Op Diff
        | RE (rack as inp)  -> Primitive(!cur, inp)
        | _ -> Nop
        in
        Stack.push thunk stack;
        process stack;
        display_top stack;
        flush stdout;
        with Stack.Empty -> printf("Nothing to do!\n");

      printf "%d\n" (Stack.length stack);
      flush stdout;
    done
      with End_of_file -> print_newline ();

