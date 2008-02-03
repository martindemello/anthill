(*pp camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                               Ledit                                 *)
(*                                                                     *)
(*       Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ledit.mli,v 1.3 1997/10/28 10:01:04 ddr Exp $ *)

value input_char : in_channel -> char;
value set_prompt : string -> unit;
value get_prompt : unit -> string;
value open_histfile : bool -> string -> unit;
value close_histfile : unit -> unit;
value set_max_len : int -> unit;
value set_son : int -> unit;
