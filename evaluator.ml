open Engine
open Environment

include Types

module Make =
  functor (Env : ENV) ->
  functor (E : ENGINE with type dict = Env.dict) ->
  struct
    open Env

    let eval env line =
      let run dict op trail = match op with
        | Anagram -> E.anagram dict trail ~all:false ~multi:false
        | Build -> E.anagram dict trail ~all:true ~multi:false
        | Pattern -> E.pattern dict trail
        | Fn s -> [s]
      in
      match line with
      | Tiles trail -> run env.dict env.op trail
      | Expr (Uop (op, trail)) -> run env.dict op trail
      | Expr (Var s) -> ["<- " ^ s]
      | Assign (v, e) -> [v ^ " <-"]
      | _ -> raise Unsupported_feature
  end
