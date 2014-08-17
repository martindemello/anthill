type group = int list;;

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

exception Unsupported_feature
