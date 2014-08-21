open MParser
open Types
open Utility

module String = Core.Std.String
module Result = Core.Std.Result

let make_group l = Group (Group.of_char_list l)
let make_uletter l = Letter (from_upper l)
let make_lletter l = Letter (from_lower l)
let make_dot l = Dot
let make_star l = Star

let make_var s = Var s

let make_uop s = 
  match (String.lowercase s) with
  | "a" | "anagram" -> Anagram
  | "p" | "pattern" -> Pattern
  | "b" | "build" -> Build
  | s -> Fn s

let make_unary_expr s t = Expr (Uop (s, t))
let make_implicit_expr s = Tiles s

let group = Tokens.squares (many1 alphanum)

let tile : (tile, unit) parser = (
      (group |>> make_group)
  <|> (Tokens.dot |>> make_dot)
  <|> (char '*' |>> make_star)
  <|> (uppercase |>> make_uletter)
  <|> (lowercase |>> make_lletter))

let term : (tiles, unit) parser = many1 tile

let fname : (string, unit) parser =
  pipe2 letter (many alphanum) (
    fun c cs -> String.of_char_list (c :: cs))

let uop : (uop, unit) parser = fname |>> make_uop

let unary : (line, unit) parser =
  pipe2 uop (spaces1 >> term) make_unary_expr

let line : (line, unit) parser =
      (attempt unary)
  <|> (term |>> make_implicit_expr)

let read_term str = parse_string term str ()

let parse s = match parse_string line s () with
| MParser.Success line -> Result.Ok line
| MParser.Failed (m, e) -> Result.Error m
