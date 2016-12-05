#######################################################################
#                                                                     #
#                           Modelicac                                 #
#                                                                     #
#          Pierre Weis, INRIA Rocquencourt                            #
#                                                                     #
#  Copyright 2010-2016,                                               #
#  Institut National de Recherche en Informatique et en Automatique.  #
#  All rights reserved.                                               #
#                                                                     #
#  This file is distributed under the terms of the BSD License.       #
#                                                                     #
#######################################################################

.PHONY: all cmo cmi cmx generated

# Basic auxiliary commands
SHELL=/bin/sh
CP=cp -pRf
CPC=cp -f

# source files

SRC_ML=\
	src/paksazi.ml \

SRC_MLI=\

# generated as by product when generating src/parser.ml

SRC_MLI_GENERATED_FILES=\

all:  $(SRC_ML_GENERATED_FILES) cmi cmo cmx bin/paksazi

bin/paksazi: bin/paksazi.byt bin/paksazi.bin 
	$(CPC) bin/paksazi.bin $@

bin/paksazi.byt: $(SRC_ML:.ml=.cmo)
	@echo "Linking paksazi.byt" && \
	$(OCAMLC) $(CAML_INCLUDES) -o $@ $(SRC_ML:.ml=.cmo)

bin/paksazi.bin: $(SRC_ML:.ml=.cmx) 
	@echo "Linking paksazi.bin" && \
	$(OCAMLOPT) $(CAML_INCLUDES) -o $@ $(SRC_ML:.ml=.cmx) 

cmi: $(SRC_MLI:.mli=.cmi) 
cmo: $(SRC_ML:.ml=.cmo)
cmx: $(SRC_ML:.ml=.cmx) 

# be sure that cmi are sequential 

CAML_INCLUDES= -I src 

# Generic clean up
EXT_TO_CLEAN=.cm* .o .a .annot .output .obj .lib

clean::
	@for EXT in $(EXT_TO_CLEAN); do\
	  find . -name "*$$EXT" -exec $(RM) "{}" \; ;\
	done

distclean:: clean

SPURIOUS_EXT_TO_CLEAN=*~ .*~ a.out .\#*

distclean::
	@for EXT in $(SPURIOUS_EXT_TO_CLEAN); do\
	  find . -name "$$EXT" -exec $(RM) "{}" \; ;\
	done

distclean::
	@$(RM) $(SRC_ML_GENERATED_FILES)
	@$(RM) $(SRC_MLI_GENERATED_FILES)

# Rebuilding dependencies

depend:: $(CAML_FILES)
	@echo generate .depend
	@$(OCAMLDEP) $(CAML_INCLUDES) $(OCAMLDEP_FLAGS) $(SRC_ML) $(SRC_MLI)  > .depend

include .depend

include ../Makefile.incl
