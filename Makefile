all: test

test:
	ocamlbuild test.native

native:
	ocamlbuild varix.native
	cp -L varix.native varix

profile:
	ocamlbuild varix.p.native

clean:
	ocamlbuild -clean
