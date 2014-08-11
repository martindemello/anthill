open MParser
include Types
include Utility

let make_group l = Group (Group.of_char_list l)
let make_uletter l = Letter (from_upper l)
let make_lletter l = Letter (from_lower l)
let make_dot l = Dot
let make_star l = Star

let group = Tokens.squares (many1 alphanum)

let tile : (tile, unit) parser = (
      (group |>> make_group)
  <|> (Tokens.dot |>> make_dot)
  <|> (char '*' |>> make_star)
  <|> (uppercase |>> make_uletter)
  <|> (lowercase |>> make_lletter))

let term : (tile list, unit) parser = many1 tile

let read_term str = parse_string term str ()
