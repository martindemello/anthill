open Printf

type node = {
  mutable eow: bool;
  mutable children: ((node option) array) option;
}

let new_node eow = {
  eow = eow;
  children = None;
}

let new_child_array () = Array.make 26 None

let implode l =
  let s = String.create (List.length l) in
  let rec f n = function
    | x :: xs -> s.[n] <- x; f (n + 1) xs
    | [] -> s
  in f 0 l

(* letter <-> index *)
let index c = (Char.code c) - 97
let letter i = Char.chr (i + 97)

let add node letters =
  let n = ref node in
  let last = String.length letters - 1 in
  let add_letter i letter =
    let eow = i == last in
    let ix = index letter in
    if !n.children == None then
      !n.children <- Some (new_child_array ());
    match !n.children with
    | None -> (); (* should never happen *)
    | Some ch -> begin
        if ch.(ix) == None then
          ch.(ix) <- Some (new_node false);
        match ch.(ix) with
        | None -> ()
        | Some node -> (node.eow <- eow; n := node)
    end
    in
    String.iteri add_letter letters

let printall node prefix =
  let rec traverse node prefix =
    if node.eow then
      Printf.printf "%s\n" (implode (List.rev prefix));
    match node.children with
    | None -> ()
    | Some ch -> 
      Array.iteri (fun i c ->
          match c with
          | None -> ()
          | Some child -> traverse child ((letter i) :: prefix)
        )
        ch
  in
  traverse node prefix

let _ =
  let root = new_node false in
  add root "hello";
  add root "world";
  printall root []











































