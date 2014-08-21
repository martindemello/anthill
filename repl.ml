open Types
open Core.Std
open Trie_search
open Utility

module Env = Environment.Make (Trie)

module Eval = Evaluator.Make (Env) (TrieEngine)

let prompt = function
  | Anagram -> "anagram > "
  | Pattern -> "pattern > "
  | Build -> "build > "
  | Fn s -> s ^ " > "

let readline prompt =
  Ledit.set_prompt prompt;
  let buf = Buffer.create 256 in
  let rec loop c = match c with
  | "\n" -> Buffer.contents buf
  | _    -> Buffer.add_string buf c; loop (Ledit.input_char stdin)
  in
  loop (Ledit.input_char stdin);;

let print_instructions () =
  print_endline "Anagram: a letters";
  print_endline "Pattern: p letters";
  print_endline "Build: b letters";
  print_endline "Use . for a blank and * for any number of blanks";
  print_endline "";
  print_endline "Hit Ctrl-D to exit";
  print_endline "------------------------------------------------";
  flush stdout

let op env expr = match expr with
| Expr (Uop (o, _)) -> o
| _ -> env.Env.op

let display_result env expr ws =
  let wlist = match (op env expr) with
  | Anagram -> sort_by caps_in ws
  | Build -> sort_by String.length ws
  | _ -> ws
  in
  List.iter wlist (printf "%s\n");
  flush stdout

let show_exc x = Printf.printf "Exception: %s\n%!" (Exn.to_string x)

let display_error e =
  printf "Error: %s\n" e;
  flush stdout

let run env str =
  let open Env in
  try
    match Parser.parse str with
    | Result.Ok expr -> begin
        let l = Eval.eval env expr in
        display_result env expr l;
        env.op <- op env expr
      end
    | Result.Error m -> display_error m
  with
  | x -> show_exc x

let repl env =
  let open Env in
  print_instructions ();
  try
    while true do
      let str = readline (prompt env.op) in
      run env str;
    done;
    flush stdout;
  with End_of_file -> print_newline ()
