RESULT = varix

SOURCES = \
					utility.ml\
					dawg.ml bag.ml sset.ml\
					search.ml \
					debug.ml \
					vx.ml varix.ml

GODI = /home/martin/opt/godi/lib/ocaml
#LIBS = unix bigarray str pcre run_mikmatch_pcre
PACKS = unix bigarray str mikmatch_pcre pcre batteries aurochs_lib
INCDIRS = $(GODI)/pkg-lib/batteries $(GODI)/std-lib/camlp5 $(GODI)/pkg-lib/pcre $(GODI)/pkg-lib/mikmatch_pcre
CREATE_LIB = yes
PRE_TARGETS = vx.ml vx.mli
USE_CAMLP4 = yes
PP = ./camlp4find $(PACKS)
export PP

all: native-code

vx.ml: vx.peg
	aurochs -target ml vx.peg

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
