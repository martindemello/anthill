type group = Group.t

type tile = Letter of int | Group of group | Dot | Star

type tiles = tile list

type args = string list

let char_of_tile n = match n with
| Letter c -> Char.chr (c + 97)
| Dot -> '.'
| Star -> '*'
| Group _ -> '?'

let tile_of_char c = match c with
| '.' -> Dot
| '*' -> Star
| c   -> Letter (Char.code c - 97)

exception Unsupported_feature

(* parser and evaluator *)

type fn =
  | Anagram
  | Build
  | Pattern
  | Multi
  | Length
  | Fn of string;;

type bop = Union | Inter | Diff | Op of string;;

type expr =
  | Words of Wordset.t
  | Var of string
  | Fun of fn * args
  | Bop of bop * expr * expr

type line =
  | Tiles of string
  | Expr of expr
  | Assign of string * expr
  | Command of fn
