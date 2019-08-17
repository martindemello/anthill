all: anthill

anthill:
	dune build main.exe
	cp _build/default/main.exe anthill

clean:
	ocamlbuild -clean
