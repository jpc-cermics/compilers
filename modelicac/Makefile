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
	src/parseTree.ml \
	src/linenum.ml \
	src/parser.ml \
	src/lexer.ml \
	src/parse.ml \
	src/precompilation.ml \
	src/compilation.ml \
	src/instantiation.ml \
	src/graphNodeSet.ml \
	src/symbolicExpression.ml \
	src/squareSparseMatrix.ml \
	src/bipartiteGraph.ml \
	src/hungarianMethod.ml \
	src/causalityGraph.ml \
	src/optimization.ml \
	src/xMLCodeGeneration.ml \
	src/optimizingCompiler.ml \
	src/scicosCodeGeneration.ml \
	src/scicosOptimizingCompiler.ml \

SRC_ML_GENERATED_FILES=\
	src/linenum.ml \
	src/parser.ml \
	src/lexer.ml \


SRC_MLI=\
	src/parseTree.mli \
	src/linenum.mli \
	src/lexer.mli \
	src/parser.mli \
	src/precompilation.mli \
	src/compilation.mli \
	src/instantiation.mli \
	src/graphNodeSet.mli \
	src/symbolicExpression.mli \
	src/squareSparseMatrix.mli \
	src/bipartiteGraph.mli \
	src/hungarianMethod.mli \
	src/causalityGraph.mli \
	src/optimization.mli \
	src/xMLCodeGeneration.mli \
	src/optimizingCompiler.mli \
	src/scicosCodeGeneration.mli \

# generated as by product when generating src/parser.ml

SRC_MLI_GENERATED_FILES=\
	src/parser.mli \

#CAML_BYT_LIBS=nums.cma
CAML_BYT_LIBS=zarith.cma

all:  $(SRC_ML_GENERATED_FILES) cmi cmo cmx bin/modelicac

bin/modelicac: bin/modelicac.byt bin/modelicac.bin 
	$(CPC) bin/modelicac.bin $@

bin/modelicac.byt: $(SRC_ML:.ml=.cmo)
	@echo "Linking modelicac.byt" && \
	$(OCAMLC) $(CAML_INCLUDES)  -a $(CAML_BYT_LIBS) \
		-o $@ $(SRC_ML:.ml=.cmo)

bin/modelicac.bin: $(SRC_ML:.ml=.cmx) 
	@echo "Linking modelicac.bin" && \
	$(OCAMLOPT) $(CAML_INCLUDES) $(CAML_BYT_LIBS:.cma=.cmxa) \
		-o $@ $(SRC_ML:.ml=.cmx) 

cmi: $(SRC_MLI:.mli=.cmi) 
cmo: $(SRC_ML:.ml=.cmo)
cmx: $(SRC_ML:.ml=.cmx) 

# be sure that cmi are sequential 


CAML_INCLUDES= -I src -I +zarith
# CAML_INCLUDES= -I src -I +num

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
	@$(RM) bin/modelicac.* bin/modelicac

# Rebuilding dependencies
# we need the generated ml and mli to exists for depend
# to be properly generated (they are inserted in the $(SRC_MLI) $(SRC_ML) list).

depend:: $(SRC_MLI) $(SRC_ML)
	@echo generate .depend
	@$(OCAMLDEP) $(CAML_INCLUDES) $(OCAMLDEP_FLAGS) $(SRC_MLI) $(SRC_ML) > .depend

include .depend

include ../Makefile.incl

# file to be generated for a proper make depend
# but which are not requested for compilation
# src/parser.mli is also requested but produced by standard rules

src/lexer.mli : src/lexer.ml src/parser.cmi
	@$(OCAMLC) -i -I src $< > $*.mli

src/linenum.mli : src/linenum.ml 
	@$(OCAMLC) -i -I src $< > $*.mli
