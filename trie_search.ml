open Utility
open Core.Std
include Types

(*************************************************************************
 * search -> trie -> wordlist
 * ***********************************************************************)

let space = 32 - 97

let explode str = String.to_list str

let trail_of_string str =
  let node_of_char chr = match chr with
  | '.' -> Dot
  | '*' -> Star
  | c   -> Letter (Char.to_int c - 97)
  in List.map (explode str) node_of_char;;

let char_of_int i = match Char.of_int (i + 97) with
  | Some c -> c
  | None -> '#'

let display sw =
  match sw with
  | None -> "[]"
  | Some w -> w

let word_of prefix = 
  Some (String.of_char_list (List.map (List.rev prefix) char_of_int))

(*************************************************************************
 * search functions starting from an arbitrary node + prefix
 * ***********************************************************************)

let collecting traversal =
  let retval = ref [] in
  let add_word = function None -> () | Some s -> retval := s :: !retval in
  traversal add_word;
  Sset.to_list (Sset.of_list !retval)

(* follow a 'trail' of characters or wildcards starting from a given prefix *)
let _pattern trie prefix trail =
  let traversal add_word =
    let rec traverse node prefix trail =
      match trail with
      | [] -> ();
      | c :: cs -> begin
        let next_step c child = follow child cs (c :: prefix) in
        match c with
        | Dot -> Trie.foreach_child node next_step
        | Star -> begin
          Trie.foreach_child node next_step;
          Trie.foreach_child node (fun c child -> follow child trail (c :: prefix))
          end
        | Letter i -> Trie.with_child node i next_step
        | Group g -> Trie.for_child_in_group node g next_step
      end
    and follow node cs prefix =
      if (List.is_empty cs && node.eow) then add_word (word_of prefix);
      traverse node prefix cs
    in
      traverse trie prefix trail
  in collecting traversal
;;

(* Build all possible words from a bag and a trie *
 * if all = false, return only words using the entire bag *)
let _build trie path bag ~all ~multi =
  let traversal add_word =
    let rec traverse node prefix =
      Trie.foreach_child node (fun c child ->
          follow child c prefix)
    and follow node c prefix =
      let letter = Mutable_rack.play bag c in 
      match letter with
      | None -> ()
      | Some played ->
        begin
          if (Mutable_rack.is_empty bag) && node.eow then
            add_word (word_of (c :: prefix))
          else
            begin
              if all && node.eow then
                add_word (word_of (c :: prefix));
              if multi && node.eow then begin
                traverse trie (space :: c :: prefix);
              end
            end;
          traverse node (c :: prefix);
          Mutable_rack.add bag played
        end
    in
    traverse trie path;
  in collecting traversal
;;

let test_multi_anags trie word =
  let bag = Mutable_rack.of_rack (trail_of_string "planted") in
  let l = _build trie [] bag ~all:false ~multi:true in
  List.iter l (fun w -> printf "%s\n" w)

let _ =
  let root = Trie.load_from_text_file "csw.lower" in
  (*let trail = trail_of_string "h.m" in*)
  let trail = [(Letter 7); Group (Group.of_string "ace"); Star; (Letter 12)] in
  let l = _pattern root [] trail in
  List.iter l (fun w -> printf "%s\n" w);
