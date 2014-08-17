open Aurochs_pack
open Env
open Peg
open Sset
open Types
open Utility
open Vx

(*************************************************************************
 * high level string -> [string] interface
 * ***********************************************************************)

let anagrams_of_rack dawg rack = Search.anagrams dawg (Bag.of_rack rack);;

let patterns_of_rack dawg rack = Search.pattern dawg rack;;

let build_of_rack dawg rack = Search.all_words dawg (Bag.of_rack rack);;


(*************************************************************************
 * parser and evaluator
 * ***********************************************************************)


let list_of_elem e = e

let set_of_elem e =
  Sset.of_list (List.map String.lowercase (list_of_elem e))

let operator_of_uop o = match o with
| Anagram -> Env.anag
| Pattern -> Env.patt
| Build -> Env.rack


(* operator parsing *)

let unary_of op =
  match (String.lowercase op) with
  | "a" -> Anagram
  | "p" -> Pattern
  | "b" -> Build
  | _ -> failwith "no such operation!"

let binary_of op =
  match (String.lowercase op) with
  | "&" | "and"  -> Inter
  | "|" | "or"   -> Union
  | "-" | "diff" -> Diff
  | _ -> failwith "no such operation!"

(* evaluation *)

let unary env o r =
  let p = match o with
  | Anagram -> anagrams_of_rack
  | Pattern -> patterns_of_rack
  | Build -> build_of_rack
  in
  p env.dawg r

let primitive env op r =
  let o = unary_of op in
  env.operator <- operator_of_uop o;
  unary env o r

let current_primitive env r =
  primitive env env.operator.op r

let binary o l r =
  let l, r = set_of_elem l, set_of_elem r in
  let s = match binary_of o with
  | Union -> StringSet.union l r
  | Inter -> StringSet.inter l r
  | Diff  -> StringSet.diff  l r
  in
  to_list s

let rack r =
  let Node(N_rack, _, tiles) = r in
  let node_of_tile tile =
    let Node(N_tile, t, _) = tile in
    match t with
    | [A_dot, _] -> Dot
    | [A_star, _] -> Star
    | [A_letter, c] -> Letter c.[0]
    | [A_group, g] ->  Group (Group.of_string g)
    | _ -> invalid_arg "Input not recognized"
  in List.map node_of_tile tiles

let lookup env v = []

let rec eval env t = match t with
  | Node(N_Root, _, [x]) -> eval env x
  | Node(N_prim, [A_uop, o], [r]) -> primitive env o (rack r)
  | Node(N_var, [A_name, v], _) -> lookup env v
  | Node(N_expr, [A_bop, o], [l; r]) -> binary o (eval env l) (eval env r)
  | Node(N_intr, _, [r]) -> current_primitive env (rack r)
  | _ -> invalid_arg "Input not recognized"
;;

(* parser *)

let parse str =
  Aurochs.read ~grammar:(`Program Vx.program) ~text:(`String str)
