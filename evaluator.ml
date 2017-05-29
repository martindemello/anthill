open Engine
open Environment
open Wordset
open Core

include Types

module Make =
  functor (Env : ENV) ->
  functor (E : ENGINE with type dict = Env.dict) ->
  struct
    open Env

    let trail args =
      match args with
      | arg :: _ -> begin
         match Parser.parse_rack arg with
           | Result.Ok trail -> trail
           | _ -> raise (Invalid_argument "none")
       end
      | _ -> raise (Invalid_argument "none")

    (* wordlist generation *)
    let prefix dict op args = match op with
      | Anagram -> E.anagram dict (trail args) ~all:false ~multi:false
      | Multi -> E.anagram dict (trail args) ~all:false ~multi:true
      | Build -> E.anagram dict (trail args) ~all:true ~multi:false
      | Pattern -> E.pattern dict (trail args)
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
      | Var v -> Wordset.of_list ["<- " ^ v]

    let eval env line =
      match line with
      | Command c -> Wordset.of_list []
      | Expr e -> expr env e
      | Assign (v, e) -> Wordset.of_list [v ^ " <-"]
      | Tiles arg -> expr env (Fun (env.op, [arg]))
  end
