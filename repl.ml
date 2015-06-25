open Types
open Core.Std
open Trie_search
open Utility
open Lwt

module Env = Environment.Make (Trie)

module Eval = Evaluator.Make (Env) (TrieEngine)

let display term text =
  let open LTerm_text in
  let out = eval [ S text ] in
  LTerm.fprintls term out

let make_prompt text =
  let open LTerm_text in
  React.S.const (eval [ S text ])

class read_line ~term ~history ~prompt = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method show_box = false

  initializer
    self#set_prompt (make_prompt prompt)
end

let prompt_of_op = function
  | Anagram -> "anagram > "
  | Multi -> "multi > "
  | Pattern -> "pattern > "
  | Build -> "build > "
  | Fn s -> s ^ " > "

let print_instructions term = display term "
  Anagram: a letters
  Pattern: p letters
  Build: b letters
  Use . for a blank and * for any number of blanks

  Hit Ctrl-D to exit
  ------------------------------------------------
  "

(* Override the active operation when an explicit unary command is entered *)
let op env expr = match expr with
| Expr (Uop (o, _)) -> o
| _ -> env.Env.op

let display_result term env expr ws =
  let ws = Wordset.to_list ws in
  let wlist = match (op env expr) with
  | Anagram -> sort_by caps_in ws
  | Build -> sort_by String.length ws
  | _ -> ws
  in
  Lwt_list.iter_s (display term) wlist

let show_exc term x =
  display term (Printf.sprintf "Exception: %s\n%!" (Exn.to_string x))

let display_error term e =
  display term ("Error: " ^ e)

let run env term str =
  let open Env in
  try
    match Parser.parse str with
    | Result.Ok expr -> begin
        let l = Eval.eval env expr in
        env.op <- op env expr;
        display_result term env expr l
      end
    | Result.Error m -> display_error term m
  with
  | x -> show_exc term x

let rec loop term history env =
  match_lwt
    try_lwt
      let prompt = prompt_of_op env.Env.op in
      let rl = new read_line ~term ~history:(LTerm_history.contents history) ~prompt in
      lwt command = rl#run in
      return (Some command)
    with Sys.Break ->
      return None
  with
  | Some command ->
      lwt () = run env term command in
      LTerm_history.add history command;
      loop term history env
  | None ->
      loop term history env

let repl env =
  lwt () = LTerm_inputrc.load () in
  try_lwt
    lwt term = Lazy.force LTerm.stdout in
    lwt () = print_instructions term in
    loop term (LTerm_history.create []) env
  with LTerm_read_line.Interrupt ->
    return ()
