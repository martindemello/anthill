module TrieEngine :
  sig
    type dict = Trie.t
    val pattern : dict -> Types.tile list -> string list
    val anagram :
      dict -> Types.tile list -> all:bool -> multi:bool -> string list
    val exists : dict -> string -> bool
  end
