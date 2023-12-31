SHELL=/bin/sh

.NOTPARALLEL:

SILENT=$(findstring -s,$(MFLAGS))

NSP=../../bin/nsp 

GNUMAKEFLAGS=--no-print-dir

SUBDIRS= modelicac paksazi translator XML2Modelica

DIR=

DESTDIR=../../bin

all clean distclean ::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ; \
	do \
		(cd $$i && if test "x$(SILENT)" != "x-s"; then echo "making $@ in $(DIR)$$i ";fi && \
		$(MAKE) $(MFLAGS) DIR=$(DIR)$$i/ $@ ); \
	   	IER=$$? &&\
	   	case $$IER in\
	    	0) ;;\
	    	*) echo "make $@ in sub directory $$i failed"; \
	       	   case '${MFLAGS}' in *[k]*) echo "carrying on compilation (-k used)";; *) exit $$IER;;esac;\
	   	esac;\
	done;

all:: $(DESTDIR)/modelicac.exe $(DESTDIR)/paksazi.exe $(DESTDIR)/translator.exe $(DESTDIR)/XML2Modelica.exe

$(DESTDIR)/modelicac.exe: modelicac/_build/default/modelicac.exe 
	@cp modelicac/_build/default/modelicac.exe $(DESTDIR)/modelicac.exe
	@chmod +x $(DESTDIR)/modelicac.exe

$(DESTDIR)/paksazi.exe: paksazi/_build/default/paksazi.exe
	@cp paksazi/_build/default/paksazi.exe $(DESTDIR)/paksazi.exe
	@chmod +x $(DESTDIR)/paksazi.exe

$(DESTDIR)/translator.exe: translator/_build/default/translator.exe
	@cp translator/_build/default/translator.exe $(DESTDIR)/translator.exe
	@chmod +x $(DESTDIR)/translator.exe

$(DESTDIR)/XML2Modelica.exe: XML2Modelica/_build/default/XML2Modelica.exe
	@cp XML2Modelica/_build/default/XML2Modelica.exe $(DESTDIR)/XML2Modelica.exe
	@chmod +x $(DESTDIR)/XML2Modelica.exe






