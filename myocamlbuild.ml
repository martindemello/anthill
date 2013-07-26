(* adapted from
   http://brion.inria.fr/gallium/index.php/Using_alphaCaml_with_ocamlbuild
*)
open Ocamlbuild_plugin;;
open Command;;

Options.use_ocamlfind := true;;

let alphaCaml = A"aurochs";;

dispatch begin function
  | After_rules ->
      rule "aurochs: peg -> ml"
        ~prods:["%.ml"; "%.mli"]
        ~dep:"%.peg"
      begin fun env _build ->
        Cmd(S[alphaCaml; A"-target"; A"ml"; P(env "%.peg")])
      end;
  | _ -> ()
end
