type group = Group.CharSet.t;;

type node = Letter of char | Group of group | Dot | Star

let char_of_node n = match n with
| Letter c -> c
| Dot -> '.'
| Star -> '*'
| Group _ -> '?'

let node_of_char c = match c with
| '.' -> Dot
| '*' -> Star
| c   -> Letter c
