open Utility
open Core.Std
open Types
open Trie_search

module Env = Environment.Make (Trie)

module Eval = Evaluator.Make (Env) (TrieEngine)

let test_pattern env str =
  match Parser.parse str with
  | Ok line -> begin
      let l = Eval.eval env line in
      let l = Wordset.to_list l in
      List.iter l (fun w -> printf "%s\n" w)
    end
  | Error m -> printf "%s\n" m

lwt () =
  let root = Trie.load_from_text_file "csw15.lower" in
  let env = { Env.dict = root; op = Anagram } in
  Repl.repl env
