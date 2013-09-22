open Utility

(* environment *)

type operator = {
  op: string;
  desc: string;
  sort: string list -> string list;
};;

type env = {
  mutable operator: operator;
  dawg: Dawg.dawg;
}

(* Default REPL environments *)
let anag, patt, rack =
  { desc = "anagram > "; op = "a"; sort = sort_by caps_in; },
  { desc = "pattern > "; op = "p"; sort = sort_by caps_in; },
  { desc = "build > "; op = "b"; sort = sort_by String.length; }
