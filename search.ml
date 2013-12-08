open Utility
include Types

(*************************************************************************
 * search -> dawg -> wordlist
 * ***********************************************************************)

let trail_of_string str =
  let node_of_char chr = match chr with
  | '.' -> Dot
  | '*' -> Star
  | c   -> Letter c
  in List.map node_of_char (explode str);;

(*************************************************************************
 * search functions starting from an arbitrary node + prefix
 * ***********************************************************************)

let collecting traversal =
  let retval = ref [] in
  let add_word = function None -> () | Some s -> retval := s :: !retval in
  traversal add_word;
  Sset.to_list (Sset.of_list !retval)

(* follow a 'trail' of characters or wildcards starting from a given prefix *)
let _pattern dawg trail path =
  let traversal add_word =
    let rec traverse trail path =
      match trail with
      | [] -> ();
      | c :: cs -> begin
        match c with
        | Dot -> Dawg.foreach_sib dawg (follow cs) path
        | Star -> ( traverse cs path; Dawg.foreach_sib dawg (follow trail) path; )
        | Letter _ -> ( try follow cs (Dawg.sib dawg c path) with Not_found -> () )
        | Group _ -> Dawg.foreach_sib_group dawg c (follow cs) path
        end
    and follow tr pth =
      let try_step tr pth = try traverse tr (Dawg.step dawg pth) with Not_found -> () in
      match tr with
      | [] -> add_word (Dawg.word_of dawg pth);
      | [Star] -> ( add_word (Dawg.word_of dawg pth); try_step tr pth; )
      | _  -> try_step tr pth
    in
      traverse trail path;
  in collecting traversal
;;

(* Build all possible words from a bag and a dawg *
 * if all = false, return only words using the entire bag *)
let _build dawg bag path ~all ~multi =
  let traversal add_word =
    let rec traverse bag path =
      Dawg.foreach_sib dawg (follow bag) path
    and follow bag path =
      let letter = Letter (Dawg.letter dawg path.Dawg.node) in
      match Bag.play letter bag with
      | _, None -> ()
      | new_bag, Some(_played) ->
          begin
            let played = char_of_node _played in
            if Bag.is_empty new_bag then add_word (Dawg.uword_of dawg path played)
            else begin
              if all then
                add_word (Dawg.uword_of dawg path played);
              if (multi && Dawg.is_word dawg path) then
                traverse new_bag (Dawg.new_word dawg path);
              try
                traverse new_bag (Dawg.ustep dawg path played)
              with Not_found -> ()
            end
          end
    in
      traverse bag path;
  in collecting traversal
;;

let _anagrams dawg bag path = _build dawg bag path ~all:false ~multi:false;;

let _all_words dawg bag path = _build dawg bag path ~all:true ~multi:false;;

let _multi_words dawg bag path = _build dawg bag path ~all:false ~multi:true;;

(*************************************************************************
 * search functions starting from the root of the dawg
 * ***********************************************************************)

let pattern dawg trail = _pattern dawg trail Dawg.start

let anagrams dawg bag = _anagrams dawg bag Dawg.start

let all_words dawg bag = _all_words dawg bag Dawg.start

let multi_words dawg bag = _multi_words dawg bag Dawg.start
