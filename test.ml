open Utility
open Core
open Types
open Top

let test_pattern env str =
  match Parser.parse str with
  | Ok line -> begin
      let l = Eval.eval env line in
      let l = Wordset.to_list l in
      List.iter l (fun w -> printf "%s\n" w)
    end
  | Error m -> printf "%s\n" m

let test_from_cmdline =
  let dict = Trie.load_from_text_file "csw15.lower" in
  let env = {(Librepl.new_env dict) with Env.op = Pattern } in
  test_pattern env "<f.t>."

lwt () =
  let dict = Trie.load_from_text_file "csw15.lower" in
  let env = ref (Librepl.new_env dict) in
  Repl.repl env
