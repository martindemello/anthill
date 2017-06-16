all: test

gui:
	ocamlbuild -cflag -g -use-ocamlfind gtkgui.native

test:
	ocamlbuild -cflag -g -use-ocamlfind test.native

native:
	ocamlbuild -use-ocamlfind varix.native
	cp -L varix.native varix

profile:
	ocamlbuild -use-ocamlfind varix.p.native

clean:
	ocamlbuild -clean
