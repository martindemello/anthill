all: trie

trie:
	ocamlbuild trie.native
	cp -L trie.native trie

native:
	ocamlbuild varix.native
	cp -L varix.native varix

profile:
	ocamlbuild varix.p.native

clean:
	ocamlbuild -clean
