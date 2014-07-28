type group = Group.CharSet.t;;

type tile = Letter of int | Group of group | Dot | Star

let char_of_tile n = match n with
| Letter c -> Char.chr (c + 97)
| Dot -> '.'
| Star -> '*'
| Group _ -> '?'

let tile_of_char c = match c with
| '.' -> Dot
| '*' -> Star
| c   -> Letter (Char.code c - 97)

(* trie node *)
type node = {
  mutable eow: bool;
  mutable children: ((node option) array) option;
}

exception Unsupported_feature
