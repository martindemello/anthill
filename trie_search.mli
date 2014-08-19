module TrieEngine :
  sig
    type dict = Trie.t
    val pattern : dict -> Types.tile list -> string list
    val anagram :
      dict -> Types.tile list -> multi:bool -> all:bool -> string list
    val exists : dict -> string -> bool
  end
