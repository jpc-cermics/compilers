#######################################################################
#                           Translator                                #
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

# Directory parsing

SRC_PARSING_ML=\
	parsing/syntax.ml\
	parsing/linenum.ml\
	parsing/location.ml\
	parsing/config.ml\
	parsing/parse_error.ml\
	parsing/parser.ml\
	parsing/lexer.ml\
	parsing/parse.ml\

SRC_PARSING_ML_GENERATED_FILES=\
	parsing/linenum.ml\
	parsing/parser.ml\
	parsing/lexer.ml\

SRC_PARSING_MLI=\
	parsing/syntax.mli\
	parsing/linenum.mli\
	parsing/location.mli\
	parsing/config.mli\
	parsing/parse_error.mli\
	parsing/parser.mli\
	parsing/lexer.mli\
	parsing/parse.mli\

SRC_PARSING_MLI_GENERATED_FILES=\
	parsing/linenum.mli\
	parsing/location.mli\
	parsing/config.mli\
	parsing/parse_error.mli\
	parsing/parser.mli\
	parsing/lexer.mli\
	parsing/parse.mli\

SRC_COMPILATION_ML=\
	compilation/types.ml\
	compilation/nameResolve.ml\

SRC_COMPILATION_MLI=\
	compilation/types.mli\
	compilation/nameResolve.mli\

# Directory exceptionHandling

SRC_EXCEPTIONHANDLING_ML=\
	exceptionHandling/errorDico.ml\
	exceptionHandling/msgDico.ml\
	exceptionHandling/exceptHandler.ml\

SRC_EXCEPTIONHANDLING_MLI=\
	exceptionHandling/errorDico.mli\
	exceptionHandling/msgDico.mli\
	exceptionHandling/exceptHandler.mli\

# Directory instantiation

SRC_INSTANTIATION_ML=\
	instantiation/instantiation.ml\

SRC_INSTANTIATION_MLI=\
	instantiation/instantiation.mli\

# Directory translation

SRC_TRANSLATION_ML=\
	translation/codeGeneration.ml\
	translation/libraryManager.ml\
	translation/versiondate.ml\
	translation/translator.ml\

SRC_TRANSLATION_MLI=\
	translation/codeGeneration.mli\
	translation/libraryManager.mli\
	translation/versiondate.mli\

# source files

SRC_ML=\
	$(SRC_PARSING_ML) \
	$(SRC_COMPILATION_ML) \
	$(SRC_EXCEPTIONHANDLING_ML) \
	$(SRC_INSTANTIATION_ML) \
	$(SRC_TRANSLATION_ML) \

SRC_ML_GENERATED_FILES=\
	$(SRC_PARSING_ML_GENERATED_FILES) \

SRC_MLI=\
	$(SRC_PARSING_MLI) \
	$(SRC_COMPILATION_MLI) \
	$(SRC_EXCEPTIONHANDLING_MLI) \
	$(SRC_INSTANTIATION_MLI) \
	$(SRC_TRANSLATION_MLI) \

SRC_MLI_GENERATED_FILES=\
	$(SRC_PARSING_MLI_GENERATED_FILES) \

all:  generated cmi cmo cmx bin/translator

generated: $(SRC_ML_GENERATED_FILES)

bin/translator: bin/translator.byt bin/translator.bin 
	$(CPC) bin/translator.bin $@

bin/translator.byt: $(SRC_ML:.ml=.cmo)
	@echo "Linking translator.byt" && \
	$(OCAMLC) $(CAML_INCLUDES)  -a $(CAML_BYT_LIBS) \
		-o $@ $(SRC_ML:.ml=.cmo)

bin/translator.bin: $(SRC_ML:.ml=.cmx) 
	@echo "Linking translator.bin" && \
	$(OCAMLOPT) $(CAML_INCLUDES) -a $(CAML_BYT_LIBS:.cma=.cmxa) \
		-o $@ $(SRC_ML:.ml=.cmx) 

cmi: $(SRC_MLI:.mli=.cmi) 
cmo: $(SRC_ML:.ml=.cmo)
cmx: $(SRC_ML:.ml=.cmx) 

# be sure that cmi are sequential 

CAML_INCLUDES=\
 -I parsing \
 -I compilation \
 -I instantiation \
 -I exceptionHandling \
 -I translation \

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

depend:: $(SRC_ML) $(SRC_MLI)
	@echo generate .depend
	@$(OCAMLDEP) $(CAML_INCLUDES) $(OCAMLDEP_FLAGS) $(SRC_ML) $(SRC_MLI)  > .depend

include .depend

include ../Makefile.incl

# files which may be generated but that we keep
# in distribution
# If the GENERATED_MLI are not present then the generated .depend
# will be different and won't work with -j
# Thus we have two possibilities
# - we keep the GENERATED_MLI and will have to update them
#   if ml exported symbols are changed
#   the make depend will then be always correct
# - we generate the mli files, build the .depend and
#   then remove the generated mli files but do not
#   rebuild the depend

generate-mli: $(SRC_MLI_GENERATED_FILES)

parsing/linenum.mli : parsing/linenum.ml 
	@$(OCAMLC) -i $(CAML_INCLUDES) $< > $*.mli

parsing/location.mli: parsing/location.ml
	@$(OCAMLC) -i $(CAML_INCLUDES) $< > $*.mli

parsing/parse_error.mli : parsing/parse_error.ml parsing/location.cmi
	@$(OCAMLC) -i $(CAML_INCLUDES) $< > $*.mli

parsing/config.mli:  parsing/config.ml  parsing/location.cmi
	@$(OCAMLC) -i $(CAML_INCLUDES) $< > $*.mli

parsing/lexer.mli : parsing/lexer.ml  parsing/config.cmi  parsing/parser.cmi parsing/parse_error.cmi
	@$(OCAMLC) -i $(CAML_INCLUDES) $< > $*.mli

parsing/parse.mli: parsing/parse.ml parsing/config.cmi parsing/parser.cmi parsing/parse_error.cmi
	@$(OCAMLC) -i $(CAML_INCLUDES) $< > $*.mli
