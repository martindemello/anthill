all: native

native:
	ocamlbuild varix.native
	cp -L varix.native varix

clean:
	ocamlbuild -clean
