RESULT = varix

SOURCES = \
					utility.ml\
					dawg.ml bag.ml \
					cursor.ml \
					ledit.mli ledit.ml \
					debug.ml \
					varix.ml

LIBS = unix bigarray str pcre run_micmatch_pcre
PACKS = unix bigarray str micmatch_pcre pcre
INCDIRS = /opt/godi/lib/ocaml/pkg-lib/pcre /opt/godi/lib/ocaml/pkg-lib/micmatch_pcre
CREATE_LIB = yes
PRE_TARGETS = pa_local.cmo
USE_CAMLP4 = yes
PP = camlp4find $(PACKS)
export PP

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
