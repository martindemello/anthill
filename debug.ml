open Printf
include Dawg

(*************************************************************************
 * debugging utilities
 * ***********************************************************************)

let display_path dawg path =
  printf "path: {%s %d:%c %s %b} "
    path.prefix path.node (Dawg.letter dawg path.node) (Dawg.word dawg path)
    (Dawg.is_word dawg path);;

let display_list pref lst =
  let rec d l =
    match l with
    [] -> printf "_";
    | c :: cs -> begin printf "%c::" c; d cs; end
  in
  printf "%s [" pref;
  d lst;
  printf "]"
;;

let display_string_list pref lst =
  let rec d l =
    match l with
    [] -> printf "_";
    | c :: cs -> begin printf "%s::" c; d cs; end
  in
  printf "%s [" pref;
  d lst;
  printf "]"
;;

let debug dawg pref trail path =
  display_list pref trail;
  display_path dawg path;
  printf("\n")
;;
