type group = Group.CharSet.t;;

type tile = Letter of char | Group of group | Dot | Star

let char_of_tile n = match n with
| Letter c -> c
| Dot -> '.'
| Star -> '*'
| Group _ -> '?'

let tile_of_char c = match c with
| '.' -> Dot
| '*' -> Star
| c   -> Letter c
