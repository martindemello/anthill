(**********************************************************
 * Global utilities (independent of the dawg)
 **********************************************************)

(**********************************************************
 * String functions 
 **********************************************************)
let explode str =
  let lst = ref [] in
  String.iter (fun c -> lst := (c :: !lst)) str;
  List.rev !lst;;

let addchar str c =
  let len = String.length str in
  let retval = String.create (len + 1) in
  String.blit str 0 retval 0 len;
  retval.[len] <- c;
  retval;;

let caps_in str =
  let lst = ref [] in
  String.iter 
  (fun c -> if ((c <= 'Z') && (c >= 'A')) then lst := (c :: !lst) else ()) 
  str;
  List.sort compare (List.rev !lst);;


(**********************************************************
 * List functions 
 **********************************************************)

let sort_by f l = 
  List.map snd (List.sort compare (List.map (fun x -> f x, x) l));;


