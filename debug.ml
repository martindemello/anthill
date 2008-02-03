open Printf
include Dawg

(*************************************************************************
 * debugging utilities
 * ***********************************************************************)

let display_path path = printf "path: {%s %d:%c %s %b} " path.prefix path.node
(letter path.node) (word path) (is_word path);;

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

let debug pref trail path = 
  display_list pref trail;
  display_path path;
  printf("\n")
;;
