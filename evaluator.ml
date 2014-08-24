open Engine
open Environment
open Wordset

include Types

module Make =
  functor (Env : ENV) ->
  functor (E : ENGINE with type dict = Env.dict) ->
  struct
    open Env

    (* wordlist generation *)
    let unary dict op trail = match op with
      | Anagram -> E.anagram dict trail ~all:false ~multi:false
      | Build -> E.anagram dict trail ~all:true ~multi:false
      | Pattern -> E.pattern dict trail
      | Fn s -> Wordset.of_list [s]

    (* binary functions *)
    let binary op l r =
      match op with
      | Union -> StringSet.union l r
      | Inter -> StringSet.inter l r
      | Diff  -> StringSet.diff  l r
      | Op _  -> raise Unsupported_feature


    let rec expr env e =
      match e with
      | Words w -> w
      | Uop (op, trail) -> unary env.dict op trail
      | Bop (op, l, r) -> binary op (expr env l) (expr env r)
      | Var v -> Wordset.of_list ["<- " ^ v]

    let eval env line =
      match line with
      | Expr e -> expr env e
      | Assign (v, e) -> Wordset.of_list [v ^ " <-"]
      | Tiles trail -> expr env (Uop (env.op, trail))
  end
