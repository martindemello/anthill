(*pp $PP *)
(*************************************************************************
 * Requires:
 *   unix.cma
 *   bigarray.cma
 *   str.cma
 *************************************************************************)

open Printf
open Debug
open Aurochs_pack
open Peg
open Vx
include Sset

(*************************************************************************
 * high level string -> [string] interface
 * ***********************************************************************)

let anagrams_of_string dawg str = Search.anagrams dawg (Bag.of_string str);;

let patterns_of_string dawg str = Search.pattern dawg (Search.trail_of_string str);;

let build_of_string dawg str = Search.all_words dawg (Bag.of_string str);;


(*************************************************************************
 * parser and evaluator
 * ***********************************************************************)

type operator = {
  desc: string;
  proc: dawg -> string -> string list;
  sort: string list -> string list;
};;

type env = {
  mutable operator: operator;
  dawg: dawg;
}

type uop = Anagram | Build | Pattern ;;
type bop = Union | Inter | Diff ;;
type rack = Rack of string;;

type elem = Nop | Primitive of uop * rack |  Words of string list

let list_of_elem e = e

let set_of_elem e =
  Sset.of_list (List.map String.lowercase (list_of_elem e))

(*************************************************************************
 * main() and friends
 * ***********************************************************************)

let anag, patt, rack =
  { desc = "anagram > "; proc = anagrams_of_string; sort = sort_by caps_in; },
  { desc = "pattern > "; proc = patterns_of_string; sort = sort_by caps_in; },
  { desc = "build > "; proc = build_of_string; sort = sort_by String.length; }

let readline prompt =
  Ledit.set_prompt prompt;
  let buf = Buffer.create 256 in
  let rec loop c = match c with
  | "\n" -> Buffer.contents buf
  | _    -> Buffer.add_string buf c; loop (Ledit.input_char stdin)
  in
  loop (Ledit.input_char stdin);;

let print_instructions () =
  print_endline "Anagram: a letters";
  print_endline "Pattern: p letters";
  print_endline "Build: b letters";
  print_endline "Use . for a blank and * for any number of blanks";
  print_endline "";
  print_endline "Hit Ctrl-D to exit";
  print_endline "------------------------------------------------";
  flush stdout

let unary_of op =
  match (String.lowercase op) with
  | "a" -> anag
  | "p" -> patt
  | "b" -> rack
  | _ -> failwith "no such operation!"

let binary_of op =
  match (String.lowercase op) with
  | "&" | "and"  -> Inter
  | "|" | "or"   -> Union
  | "-" | "diff" -> Diff
  | _ -> failwith "no such operation!"

let primitive env x y =
  let op = unary_of x in
  env.operator <- op;
  op.proc env.dawg y

let binary o l r =
  let l, r = set_of_elem l, set_of_elem r in
  let s = match binary_of o with
  | Union -> StringSet.union l r
  | Inter -> StringSet.inter l r
  | Diff  -> StringSet.diff  l r
  in
  to_list s

let lookup env v = []

let rec eval env t = match t with
  | Node(N_Root, _, [x])                   -> eval env x
  | Node(N_prim, [A_uop, o; A_rack, r], _) -> primitive env o r
  | Node(N_var, [A_name, v], _)            -> lookup env v
  | Node(N_expr, [A_bop, o], [l; r])       -> binary o (eval env l) (eval env r)
  | Node(N_intr, [A_rack, r], [])          -> env.operator.proc env.dawg r
  | _ -> invalid_arg "Input not recognized"
;;

let display_result ws =
  List.iter (printf "%s\n") ws;
  flush stdout

let show_exc x = Printf.printf "Exception: %s\n%!" (Printexc.to_string x)

let bad_command () =
  printf "Bad command\n";
  flush stdout

let parse env str =
  try
    let t = Aurochs.read ~grammar:(`Program Vx.program) ~text:(`String str) in
    let x = eval env t in
    display_result x
  with
  | Invalid_argument a -> bad_command ()
  | Aurochs.Parse_error b -> bad_command ()
  | x  -> show_exc x

let _ =
  let dawg = Dawg.load "csw.dwg" in
  let env = {operator = anag; dawg = dawg} in
  if (Array.length Sys.argv == 1) then begin
    print_instructions ();
    try
      while true do
        let str = readline env.operator.desc in
        parse env str;
      done;
      flush stdout;
    with End_of_file -> print_newline ();
  end else begin
    let trail = [(Letter 'h'); Group (Group.of_string "ace"); (Letter 'm')] in
    display_result (Search.pattern env.dawg trail);

    let bag = Bag.of_string "pink." in
    display_result (Search.anagrams env.dawg bag)

  end


