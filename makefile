RESULT = varix

SOURCES = \
					utility.ml\
					dawg.ml bag.ml sset.ml\
					search.ml \
					debug.ml \
					ledit/cursor.ml \
					ledit/ledit.mli ledit/ledit.ml \
					varix.ml

LIBS = unix bigarray str pcre run_mikmatch_pcre
PACKS = unix bigarray str mikmatch_pcre pcre
INCDIRS = /usr/lib/ocaml/camlp5 /usr/lib/ocaml/pcre-ocaml /usr/lib/ocaml/site-lib/mikmatch_pcre # /opt/godi/lib/ocaml/pkg-lib/pcre /opt/godi/lib/ocaml/pkg-lib/micmatch_pcre
CREATE_LIB = yes
PRE_TARGETS = ledit/pa_local.cmo ledit/pa_def.cmo
USE_CAMLP4 = yes
PP = ./camlp4find $(PACKS)
export PP

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
