binpath		:= $(HOME)/bin

sources		:= Main.lhs TeXCommands.lhs TeXParser.lhs \
		   Typewriter.lhs Math.lhs\
		   Directives.lhs HsLexer.lhs \
		   Parser.lhs FiniteMap.lhs Auxiliaries.lhs \
		   StateT.lhs Document.lhs Verbatim.lhs Value.lhs
snips		:= sorts.tt sorts.math id.math cata.math spec.math
objects         := $(foreach file, $(sources:.lhs=.o), $(file))
sections       	:= $(foreach file, $(sources:.lhs=.tex), $(file))
include lhs2TeX.d
ghc		:= ghc
ghcflags	:= -Wall -O -recomp -package lang
$(objects) : %.o : %.lhs
	$(ghc) -c $(ghcflags) $< -o $@

%.hi : %.o
	@if [ ! -f $@ ] ; then \
	echo $(RM) $< ; \
	$(RM) $< ; \
	set +e ; \
	echo $(MAKE) $(notdir $<) ; \
	$(MAKE) $(notdir $<) ; \
	if [ $$? -ne 0 ] ; then \
	exit 1; \
	fi ; \
	fi

%.tex : %.lhs lhs2TeX Lhs2TeX.fmt
#	lhs2TeX -verb -iLhs2TeX.fmt $< > $@
	./lhs2TeX -math -align 33 -iLhs2TeX.fmt $< > $@

%.tt : %.snip lhs2TeX lhs2TeX.fmt
	./lhs2TeX -tt -lmeta=True -ilhs2TeX.fmt $< > $@

%.math : %.snip lhs2TeX lhs2TeX.fmt
	./lhs2TeX -math -align 33 -lmeta=True -ilhs2TeX.fmt $< > $@

%.tex : %.lit lhs2TeX
	./lhs2TeX -verb -ilhs2TeX.fmt $< > $@

%.ps : %.dvi
	dvips -D600 -o $@ $<

.PHONY: depend xdvi gv print install backup clean

lhs2TeX.sty: lhs2TeX.sty.lit
	./lhs2TeX -code lhs2TeX.sty.lit > lhs2TeX.sty
lhs2TeX.fmt: lhs2TeX.fmt.lit
	./lhs2TeX -code lhs2TeX.fmt.lit > lhs2TeX.fmt

lhs2TeX : $(objects)
	$(ghc) $(ghcflags) -o lhs2TeX $(objects)

depend:
	mkdependHS -f lhs2TeX.d -- $(ghcflags) -- $(sources)
	rm -f lhs2TeX.d.bak

Lhs2TeX.dvi : Lhs2TeX.tex lhs2TeX.sty $(sections) $(snips) lhs2TeX.sty.tex lhs2TeX.fmt.tex Makefile.tex
	latex Lhs2TeX
#	latex Lhs2TeX

xdvi : Lhs2TeX.dvi
	xdvi -s 3 Lhs2TeX.dvi &

gv : Lhs2TeX.ps
	gv Lhs2TeX.ps &

print : Lhs2TeX.dvi
	dvips -D600 -f Lhs2TeX.dvi | lpr -Pa -Zl

install : lhs2TeX lhs2TeX.sty lhs2TeX.fmt
	mv lhs2TeX $(binpath)

backup:
	cd ..; \
	rm -f Literate.tar Literate.tar.gz; \
	tar -cf Literate.tar Literate; \
	gzip Literate.tar; \
	chmod a+r Literate.tar.gz

clean :
	clean
	rm -f lhs2TeX $(sections) $(snips) $(objects) *.hi *.dvi *.ps
	rm -rf lhs2TeX.sty lhs2TeX.fmt
	rm -rf Lhs2TeX.tex lhs2TeX.sty.tex lhs2TeX.fmt.tex Makefile.tex 

all:
	$(MAKE) install
	$(MAKE) Lhs2TeX.dvi







