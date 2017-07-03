all: test

gui:
	ocamlbuild -cflag -g -use-ocamlfind gtkgui.native

test:
	ocamlbuild -cflag -g -use-ocamlfind test.native

native:
	ocamlbuild -use-ocamlfind hex.native
	cp -L hex.native hex

profile:
	ocamlbuild -use-ocamlfind hex.p.native

clean:
	ocamlbuild -clean
