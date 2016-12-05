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

$(DESTDIR)/modelicac.exe: modelicac/bin/modelicac.bin 
	@cp modelicac/bin/modelicac.bin $(DESTDIR)/modelicac.exe
	@chmod +x $(DESTDIR)/modelicac.exe

$(DESTDIR)/paksazi.exe: paksazi/bin/paksazi.bin
	@cp paksazi/bin/paksazi.bin $(DESTDIR)/paksazi.exe
	@chmod +x $(DESTDIR)/paksazi.exe

$(DESTDIR)/translator.exe: translator/bin/translator.bin
	@cp translator/bin/translator.bin $(DESTDIR)/translator.exe
	@chmod +x $(DESTDIR)/translator.exe

$(DESTDIR)/XML2Modelica.exe: XML2Modelica/bin/XML2Modelica.bin
	@cp XML2Modelica/bin/XML2Modelica.bin $(DESTDIR)/XML2Modelica.exe
	@chmod +x $(DESTDIR)/XML2Modelica.exe






