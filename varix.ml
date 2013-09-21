(*pp $PP *)
(*************************************************************************
 * Requires:
 *   unix.cma
 *   bigarray.cma
 *   str.cma
 *************************************************************************)

open Printf
open Debug
open Aurochs_pack


(*************************************************************************
 * main() and friends
 * ***********************************************************************)

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

let display_result ws =
  List.iter (printf "%s\n") ws;
  flush stdout

let show_exc x = Printf.printf "Exception: %s\n%!" (Printexc.to_string x)

let bad_command () =
  printf "Bad command\n";
  flush stdout

let run env str =
  try
    let open Eval in
    let t = parse str in
    let x = eval env t in
    display_result (env.operator.sort x)
  with
  | Invalid_argument a -> bad_command ()
  | Aurochs.Parse_error b -> bad_command ()
  | x  -> show_exc x

let repl env =
  let open Eval in
  print_instructions ();
  try
    while true do
      let str = readline env.operator.desc in
      run env str;
    done;
    flush stdout;
  with End_of_file -> print_newline ()

let run_test env =
  let open Eval in
  let trail = [(Letter 'h'); Group (Group.of_string "ace"); (Letter 'm')] in
  display_result (Search.pattern env.dawg trail);

  let bag = Bag.of_string "pink." in
  display_result (Search.anagrams env.dawg bag)

let _ =
  let open Eval in
  let dawg = Dawg.load "csw.dwg" in
  let env = {operator = anag; dawg = dawg} in
  if (Array.length Sys.argv == 1) then
    repl env
  else
    run_test env
