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
	src/linenum.ml\
	src/stringLexer.ml\
	src/stringParser.ml\
	src/modelicaCodeGenerator.ml\
	src/xMLLexer.ml\
	src/xMLParser.ml\
	src/xMLTree.ml\
	src/xML2Modelica.ml\

SRC_ML_GENERATED_FILES=\
	src/linenum.ml\
	src/stringLexer.ml\
	src/stringParser.ml\
	src/xMLLexer.ml\
	src/xMLParser.ml\

SRC_MLI=\
	src/modelicaCodeGenerator.mli\
	src/xMLTree.mli\
	src/linenum.mli\
	src/stringLexer.mli\
	src/xMLLexer.mli\
	src/stringParser.mli\
	src/xMLParser.mli\

# generated as by product when generating src/parser.ml

SRC_MLI_GENERATED_FILES=\

all:  generated cmi cmo cmx bin/XML2Modelica

generated: $(SRC_ML_GENERATED_FILES)

bin/XML2Modelica: bin/XML2Modelica.byt bin/XML2Modelica.bin 
	$(CPC) bin/XML2Modelica.bin $@

bin/XML2Modelica.byt: $(SRC_ML:.ml=.cmo)
	@echo "Linking XML2Modelica.byt" && \
	$(OCAMLC) $(CAML_INCLUDES)  -o $@ $(SRC_ML:.ml=.cmo)

bin/XML2Modelica.bin: $(SRC_ML:.ml=.cmx) 
	@echo "Linking XML2Modelica.bin" && \
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

distclean::
	@$(RM) bin/XML2Modelica.* bin/XML2Modelica

# Rebuilding dependencies

depend:: $(SRC_ML) $(SRC_MLI)
	@echo generate .depend
	@$(OCAMLDEP) $(CAML_INCLUDES) $(OCAMLDEP_FLAGS) $(SRC_ML) $(SRC_MLI)  > .depend

include .depend

include ../Makefile.incl

# files which may be generated but that we keep
# in distribution to simplify the rules.

src/stringLexer.mli : src/stringLexer.ml src/stringParser.cmi
	@$(OCAMLC) -i -I src $< > $*.mli

src/linenum.mli : src/linenum.ml 
	@$(OCAMLC) -i -I src $< > $*.mli

src/xMLLexer.mli : src/xMLLexer.ml src/xMLParser.cmi
	@$(OCAMLC) -i -I src $< > $*.mli
