ocamlopt -a bag.ml -o bag.cma
ocamlopt unix.cmxa bigarray.cmxa cursor.cmx ledit.cmx bag.cmx dawgutils.ml -o dawgutils
