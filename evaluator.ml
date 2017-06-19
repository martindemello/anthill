open Engine
open Environment
open Wordset
open Core
open Utility

include Types

module Make =
  functor (Env : ENV) ->
  functor (E : ENGINE with type dict = Env.dict) ->
  struct
    open Env

    (* argument conversion *)

    (* validate that list has only letters and dots *)
    let validate_letter_dot cs =
      List.for_all cs (fun c -> (Char.is_lowercase c) || (c = '.'))

    (* validate that list has only letters and exactly one dot *)
    let validate_fit cs =
      validate_letter_dot cs && 
      ((List.count cs (fun c -> c = '.')) = 1)

    let expand_fit dict cs =
      let fn = function
          '.' -> Dot
        | c -> Letter (from_lower c)
      in
      if validate_fit cs then
        let pat = List.map cs fn in
        let chars = E.fit dict pat in
        Group (Group.of_char_list chars)
      else
        raise (Invalid_argument "<> group can only have letters and a single .")

    let expand_group dict f =
      match f with
        Fit cs -> expand_fit dict cs

    let expand_rack dict ts =
      let expand t = match t with
        | Final x -> x
        | Expand f -> expand_group dict f
      in
      List.map ts expand

    let single_arg args =
      match args with
      | [arg] -> Ok arg
      | _ -> Error "Expected: Single argument"

    let trail dict args =
      let open Rresult.R in
      (single_arg args >>= Parser.parse_rack)
      |> get_ok
      |> expand_rack dict

    let length_pattern args =
      let open Rresult.R in
      let n = (single_arg args >>= Parser.parse_int) |> get_ok in
      List.init n (fun _ -> Dot)

    let overlap_pattern pos dict arg =
      let chars = String.to_list arg in
      let pat c = match pos with
          `Above -> ['.'; c]
        | `Below -> [c; '.']
      in
      let exp c = match c with
        | '.' -> Dot
        | _ -> expand_fit dict (pat c)
      in
      let groups = List.map ~f:exp chars in
      groups

    let above_pattern = overlap_pattern `Above

    let below_pattern = overlap_pattern `Below

    (* prefix functions *)
    let fn_anagram dict args =
      E.anagram dict (trail dict args) ~all:false ~multi:false

    let fn_multi dict args =
      E.anagram dict (trail dict args) ~all:false ~multi:true

    let fn_build dict args =
      E.anagram dict (trail dict args) ~all:true ~multi:false

    let fn_pattern dict args =
      E.pattern dict (trail dict args)

    let fn_length dict args =
      E.pattern dict (length_pattern args)

    let fn_above dict args =
      E.pattern dict (above_pattern dict (List.hd_exn args))

    let fn_below dict args =
      E.pattern dict (below_pattern dict (List.hd_exn args))

    (* wordlist generation *)
    let prefix dict op args = match op with
      | Anagram -> fn_anagram dict args
      | Multi -> fn_multi dict args
      | Build -> fn_build dict args
      | Pattern -> fn_pattern dict args
      | Length -> fn_length dict args
      | Above -> fn_above dict args
      | Below -> fn_below dict args
      | Fn s -> Wordset.of_list [s]

    (* binary functions *)
    let binary op l r =
      let (l, r) = (Wordset.to_lower l, Wordset.to_lower r) in
      match op with
      | Union -> Wordset.union l r
      | Inter -> Wordset.inter l r
      | Diff  -> Wordset.diff  l r
      | Op _  -> raise Unsupported_feature

    let rec expr env e =
      match e with
      | Words w -> w
      | Fun (op, args) -> prefix env.dict op args
      | Bop (op, l, r) -> binary op (expr env l) (expr env r)
      | Var v -> Vars.get env.vars v

    let eval env line =
      match line with
      | Command c -> env, Wordset.of_list []
      | Expr e -> env, expr env e
      | Tiles arg -> env, expr env (Fun (env.op, [arg]))
      | Assign (v, e) -> begin
          let ws = expr env e in
          let env' = {env with vars = (Vars.set env.vars v ws)} in
          env', ws
        end
  end
