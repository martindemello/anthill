open Dawg

(*************************************************************************
 * search -> dawg -> wordlist
 * ***********************************************************************)

(* check if a word is good *)
let check str =
  let len = (String.length str - 1) in
  let rec walk n i =
    try
      let p = find str.[i] n in
      if i = len then (wordp p) else (walk (ptr p) (i+1))
    with Not_found -> false
  in
  walk start_node 0;;

(* follow a 'trail' of characters or wildcards starting from a given prefix *)
let pattern trail path =
  let retval = ref [] in
  let add_word = function [] -> () | s::_ -> retval := s :: !retval in
  let rec traverse trail path =
    match trail with
    |[]        -> ();
    |'.' :: cs -> foreach_sib (follow cs) path
    |'*' :: cs -> ( traverse cs path; foreach_sib (follow trail) path; )
    |c   :: cs -> try follow cs (sib c path) with Not_found -> ()
  and follow tr pth =
    let try_step tr pth = try traverse tr (step pth) with Not_found -> () in
    match tr with
    |[]    -> add_word (word_of pth);
    |['*'] -> ( add_word (word_of pth); try_step tr pth; )
    |_     -> try_step tr pth
  in
    traverse trail path;
    List.rev !retval
;;

(* Build all possible words from a bag and a dawg *
 * if all = false, return only words using the entire bag *)
let build bag path all =
  let retval = ref [] in
  let add_word = function [] -> () | s::_ -> retval := s :: !retval in
  let rec traverse bag path =
    foreach_sib (follow bag) path
  and follow bag path =
    let new_bag, played = Bag.play (letter path.node) bag in
    if Bag.is_empty new_bag then add_word (uword_of path played)
    else
      (if all then add_word (uword_of path played) else ());
      try traverse new_bag (ustep path played) with Not_found -> ()
  in
    traverse bag path;
    List.rev !retval
;;

let anagrams bag path = build bag path false;;

let all_words bag path = build bag path true;;
