open Printf
open Core.Std

type node = {
  mutable eow: bool;
  mutable children: ((node option) array) option;
}

let new_node eow = {
  eow = eow;
  children = None;
}

let new_child_array () = Array.create 26 None

let is_none n = phys_equal n None

(* letter <-> index *)
let index c = (Char.to_int c) - 97
let letter i = match Char.of_int (i + 97) with
  | Some c -> c
  | None -> '#'

let add node letters =
  let n = ref node in
  let add_letter letter =
    let ix = index letter in
    if is_none !n.children then
      !n.children <- Some (new_child_array ());
    match !n.children with
    | None -> (); (* should never happen *)
    | Some ch -> begin
        if is_none ch.(ix) then
          ch.(ix) <- Some (new_node false);
        match ch.(ix) with
        | None -> ()
        | Some node -> n := node
    end
    in
    String.iter letters add_letter;
    !n.eow <- true

let printall node prefix =
  let rec traverse node prefix =
    if node.eow then
      Printf.printf "%s\n" (String.of_char_list (List.rev prefix));
    match node.children with
    | None -> ()
    | Some ch -> 
      Array.iteri (fun i c ->
          match c with
          | None -> ()
          | Some child -> traverse child ((letter i) :: prefix)
        ) ch
  in
  traverse node prefix

let _ =
  let root = new_node false in
  let words = In_channel.read_lines "csw.lower" in
  List.iter words (fun w -> add root w);
  printall root []











































