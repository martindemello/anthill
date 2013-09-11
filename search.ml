(*************************************************************************
 * search -> dawg -> wordlist
 * ***********************************************************************)

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
      |[]        -> ();
      |'.' :: cs -> Dawg.foreach_sib dawg (follow cs) path
      |'*' :: cs -> ( traverse cs path; Dawg.foreach_sib dawg (follow trail) path; )
      |c   :: cs -> try follow cs (Dawg.sib dawg c path) with Not_found -> ()
    and follow tr pth =
      let try_step tr pth = try traverse tr (Dawg.step dawg pth) with Not_found -> () in
      match tr with
      |[]    -> add_word (Dawg.word_of dawg pth);
      |['*'] -> ( add_word (Dawg.word_of dawg pth); try_step tr pth; )
      |_     -> try_step tr pth
    in
      traverse trail path;
  in collecting traversal
;;

(* Build all possible words from a bag and a dawg *
 * if all = false, return only words using the entire bag *)
let _build dawg bag path all =
  let traversal add_word =
    let rec traverse bag path =
      Dawg.foreach_sib dawg (follow bag) path
    and follow bag path =
      try
        let new_bag, played = Bag.play (Dawg.letter dawg path.Dawg.node) bag in
        if Bag.is_empty new_bag then add_word (Dawg.uword_of dawg path played)
        else
          (if all then add_word (Dawg.uword_of dawg path played) else ());
          traverse new_bag (Dawg.ustep dawg path played)
      with Not_found -> ()
    in
      traverse bag path;
  in collecting traversal
;;

let _anagrams dawg bag path = _build dawg bag path false;;

let _all_words dawg bag path = _build dawg bag path true;;

(*************************************************************************
 * search functions starting from the root of the dawg
 * ***********************************************************************)

let pattern dawg trail = _pattern dawg trail Dawg.start

let anagrams dawg bag = _anagrams dawg bag Dawg.start

let all_words dawg  bag = _all_words dawg bag Dawg.start
