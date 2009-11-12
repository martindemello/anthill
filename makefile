RESULT = varix

SOURCES = \
					utility.ml\
					dawg.ml bag.ml sset.ml\
					search.ml \
					debug.ml \
					ledit/cursor.ml \
					ledit/ledit.mli ledit/ledit.ml \
					varix.ml

GODI = /home/martin/opt/godi/lib/ocaml
#LIBS = unix bigarray str pcre run_mikmatch_pcre
PACKS = unix bigarray str mikmatch_pcre pcre batteries 
INCDIRS = $(GODI)/pkg-lib/batteries $(GODI)/std-lib/camlp5 $(GODI)/pkg-lib/pcre $(GODI)/pkg-lib/mikmatch_pcre
CREATE_LIB = yes
PRE_TARGETS = ledit/pa_local.cmo ledit/pa_def.cmo
USE_CAMLP4 = yes
PP = ./camlp4find $(PACKS)
export PP

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
