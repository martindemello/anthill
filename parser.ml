open Types
open Utility
open MParser
open MParser_PCRE
open Tokens

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
  | "m" | "multi" -> Multi
  | "p" | "pattern" -> Pattern
  | "b" | "build" -> Build
  | s -> Fn s

let make_bop s =
  match s with
  | "|" -> Union
  | "&" -> Inter
  | "-" -> Diff
  | s   -> Op s

let make_binary o l r = Bop ((make_bop o), l, r)

let make_var s = Var s
let make_unary s t = Uop (s, t)

let make_expr e = Expr e
let make_assign v e = Assign (v, e)
let make_implicit_expr s = Tiles s

let group = squares (many1 alphanum)

let tile : (tile, unit) parser = (
      (group |>> make_group)
  <|> (dot |>> make_dot)
  <|> (char '*' |>> make_star)
  <|> (uppercase |>> make_uletter)
  <|> (lowercase |>> make_lletter))

let rack : (tiles, unit) parser = many1 tile

let name : (string, unit) parser =
  pipe2 letter (many alphanum) (
    fun c cs -> String.of_char_list (c :: cs))

let uop : (uop, unit) parser = name |>> make_uop

let varname : (string, unit) parser = char '$' >> name
let var : (expr, unit) parser = varname |>> make_var

let unary : (expr, unit) parser =
  pipe2 uop (spaces1 >> rack) make_unary

let expr : (expr, unit) parser = unary <|> var

let infix sym : (expr, unit) operator =
  Infix (spaces >> skip_string sym >> spaces >> return (make_binary sym),
         Assoc_none)

let operators =
  [
    [ infix "&";
      infix "|";
      infix "-";
    ];
  ]

let rec term s = (parens exp <|> expr) s
    and exp s = expression operators term s

let line_expr : (line, unit) parser = exp |>> make_expr

let lhs : (string, unit) parser = (varname << spaces << char '=' << spaces)

let assign : (line, unit) parser = pipe2 lhs expr make_assign

let line : (line, unit) parser =
      (attempt assign)
  <|> (attempt line_expr)
  <|> (rack |>> make_implicit_expr)

let input : (line, unit) parser = line << eof

let parse s = match parse_string input s () with
| MParser.Success line -> Result.Ok line
| MParser.Failed (m, e) -> Result.Error m
