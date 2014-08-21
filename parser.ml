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

let make_uop s = 
  match (String.lowercase s) with
  | "a" | "anagram" -> Anagram
  | "p" | "pattern" -> Pattern
  | "b" | "build" -> Build
  | s -> Fn s

let make_var s = Var s
let make_unary s t = Uop (s, t)

let make_expr e = Expr e
let make_assign v e = Assign (v, e)
let make_implicit_expr s = Tiles s

let group = Tokens.squares (many1 alphanum)

let tile : (tile, unit) parser = (
      (group |>> make_group)
  <|> (Tokens.dot |>> make_dot)
  <|> (char '*' |>> make_star)
  <|> (uppercase |>> make_uletter)
  <|> (lowercase |>> make_lletter))

let term : (tiles, unit) parser = many1 tile

let name : (string, unit) parser =
  pipe2 letter (many alphanum) (
    fun c cs -> String.of_char_list (c :: cs))

let uop : (uop, unit) parser = name |>> make_uop

let varname : (string, unit) parser = char '$' >> name
let var : (expr, unit) parser = varname |>> make_var

let unary : (expr, unit) parser =
  pipe2 uop (spaces1 >> term) make_unary

let expr : (expr, unit) parser = unary <|> var

let line_expr : (line, unit) parser = expr |>> make_expr

let lhs : (string, unit) parser = (varname << spaces << char '=' << spaces)

let assign : (line, unit) parser = pipe2 lhs expr make_assign

let line : (line, unit) parser =
      (attempt assign)
  <|> (attempt line_expr)
  <|> (term |>> make_implicit_expr)

let input : (line, unit) parser = line << eof

let read_term str = parse_string term str ()

let parse s = match parse_string input s () with
| MParser.Success line -> Result.Ok line
| MParser.Failed (m, e) -> Result.Error m
