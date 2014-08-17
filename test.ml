open Utility
open Core.Std
open Types
open Trie_search

let explode str = String.to_list str

let trail_of_string str =
  let node_of_char chr = match chr with
  | '.' -> Dot
  | '*' -> Star
  | c   -> Letter (Char.to_int c - 97)
  in List.map (explode str) node_of_char;;

let test_multi_anags trie word =
  let rack = trail_of_string "planted" in
  let l = TrieEngine.anagram trie rack ~all:false ~multi:true in
  List.iter l (fun w -> printf "%s\n" w)

let test_pattern trie str =
  match Parser.read_term str with
  | MParser.Success trail -> begin
      let l = TrieEngine.pattern trie trail in
      List.iter l (fun w -> printf "%s\n" w)
    end
  | MParser.Failed (m, e) -> printf "%s\n" m


let _ =
  let root = Trie.load_from_text_file "csw.lower" in
  test_pattern root "h[ace]*m"
