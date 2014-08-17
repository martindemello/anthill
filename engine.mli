open Types

module type ENGINE = sig
  type dict 

  val pattern : dict -> tile list -> string list

  val anagram : dict -> tile list -> multi:bool -> all:bool -> string list

  val exists : dict -> string -> bool
end
