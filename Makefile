
include config.mk

psources        := Main.lhs TeXCommands.lhs TeXParser.lhs \
		   Typewriter.lhs Math.lhs MathPoly.lhs \
                   NewCode.lhs \
		   Directives.lhs HsLexer.lhs FileNameUtils.lhs \
		   Parser.lhs FiniteMap.lhs Auxiliaries.lhs \
		   StateT.lhs Document.lhs Verbatim.lhs Value.lhs
sources         := $(psources) Version.lhs
snipssrc        := sorts.snip id.snip cata.snip spec.snip
snips	        := sorts.tt sorts.math id.math cata.math spec.math
objects         := $(sources:.lhs=.o)
sections       	:= $(sources:.lhs=.tex)

MKINSTDIR       := ./mkinstalldirs

###
### lhs dependencies (from %include lines)
###

MKLHSDEPEND = $(GREP) "^%include " $< \
               | $(SED) -e 's,^%include ,$*.tex : ,' \
               | $(SORT) | $(UNIQ) > $*.ld

MKFMTDEPEND = $(GREP) "^%include " $< \
               | $(SED) -e 's,^%include ,$*.fmt : ,' \
               | $(SORT) | $(UNIQ) > $*.ld

###
### hs dependencies
###


MKGHCDEPEND = $(GHC) -M -optdep-f -optdep$*.d $(GHCFLAGS) $<

###
### dependency postprocessing
###

DEPPOSTPROC = $(SED) -e 's/\#.*//' -e 's/^[^:]*: *//' -e 's/ *\\$$//' \
	         -e '/^$$/ d' -e 's/$$/ :/'

###
### default targets
###

.PHONY : default xdvi gv print install backup clean all depend bin doc srcdist

all : default

default : bin doc
bin : lhs2TeX lhs2TeX.fmt lhs2TeX.sty

%.d : %.lhs
	$(MKGHCDEPEND); \
	$(CP) $*.d $*.dd; \
	$(DEPPOSTPROC) < $*.dd >> $*.d; \
	$(RM) -f $*.dd

$(objects) : %.o : %.lhs
	$(GHC) -c $(GHCFLAGS) $< -o $@

-include $(sources:%.lhs=%.d)

# I don't understand this ... (ks)
#
# %.hi : %.o
# 	@if [ ! -f $@ ] ; then \
#	echo $(RM) $< ; \
#	$(RM) $< ; \
#	set +e ; \
#	echo $(MAKE) $(notdir $<) ; \
#	$(MAKE) $(notdir $<) ; \
#	if [ $$? -ne 0 ] ; then \
#	exit 1; \
#	fi ; \
#	fi

%.hi : %.o
	@:

%.ld : %.lhs
	$(MKLHSDEPEND); \
	$(CP) $*.ld $*.ldd; \
	$(DEPPOSTPROC) < $*.ldd >> $*.ld; \
	$(RM) -f $*.ldd

%.ld : %.fmt
	$(MKFMTDEPEND); \
	$(CP) $*.ld $*.ldd; \
	$(DEPPOSTPROC) < $*.ldd >> $*.ld; \
	$(RM) -f $*.ldd

%.tex : %.lhs lhs2TeX Lhs2TeX.fmt lhs2TeX.fmt
#	lhs2TeX -verb -iLhs2TeX.fmt $< > $@
	./lhs2TeX --math --align 33 -iLhs2TeX.fmt $< > $@

-include $(sources:%.lhs=%.ld)

%.tt : %.snip lhs2TeX lhs2TeX.fmt
	./lhs2TeX --tt -lmeta=True -ilhs2TeX.fmt $< > $@

%.math : %.snip lhs2TeX lhs2TeX.fmt
	./lhs2TeX --math --align 33 -lmeta=True -ilhs2TeX.fmt $< > $@

%.tex : %.lit lhs2TeX
	./lhs2TeX --verb -ilhs2TeX.fmt $< > $@


lhs2TeX.sty: lhs2TeX.sty.lit lhs2TeX
	./lhs2TeX --code lhs2TeX.sty.lit > lhs2TeX.sty
lhs2TeX.fmt: lhs2TeX.fmt.lit lhs2TeX
	./lhs2TeX --code lhs2TeX.fmt.lit > lhs2TeX.fmt

lhs2TeX : $(objects)
	$(GHC) $(GHCFLAGS) -o lhs2TeX $(objects)

doc : bin
	cd doc; $(MAKE)
#	cd Guide; $(MAKE) Guide.pdf

depend:
	$(GHC) -M -optdep-f -optdeplhs2TeX.d $(GHCFLAGS) $(sources)
	$(RM) -f lhs2TeX.d.bak

lhs2TeX-includes : lhs2TeX.sty $(sections) $(snips) lhs2TeX.sty.tex lhs2TeX.fmt.tex Makefile.tex

Lhs2TeX.dvi : lhs2TeX-includes
Lhs2TeX.pdf : lhs2TeX-includes

xdvi : Lhs2TeX.dvi
	$(XDVI) -s 3 Lhs2TeX.dvi &

gv : Lhs2TeX.ps
	$(GV) Lhs2TeX.ps &

print : Lhs2TeX.dvi
	$(DVIPS) -D600 -f Lhs2TeX.dvi | lpr -Pa -Zl

install : bin
	$(MKINSTDIR) $(DESTDIR)$(bindir)
	$(INSTALL) -m 755 lhs2TeX $(DESTDIR)$(bindir)
	$(MKINSTDIR) $(DESTDIR)$(stydir)
	$(INSTALL) -m 644 lhs2TeX.sty lhs2TeX.fmt $(DESTDIR)$(stydir)
# TODO: install documentation
ifeq ($(INSTALL_POLYTABLE),yes)
# install polytable package
	$(MKINSTDIR) $(DESTDIR)$(polydir)
	$(INSTALL) -m 644 polytable/*.sty $(DESTDIR)$(polydir)
ifndef DESTDIR
	$(MKTEXLSR)
else
	echo "Please update the TeX filename database."
endif
endif

srcdist : doc
	if test -d $(DISTDIR); then $(RM) -rf $(DISTDIR); fi
	$(MKINSTDIR) $(DISTDIR)
	$(MKINSTDIR) $(DISTDIR)/doc
	$(MKINSTDIR) $(DISTDIR)/polytable
	$(MKINSTDIR) $(DISTDIR)/Testsuite
	$(MKINSTDIR) $(DISTDIR)/Examples
	$(INSTALL) -m 644 $(psources) Version.lhs.in $(snipssrc) $(DISTDIR)
	$(INSTALL) -m 644 lhs2TeX.fmt.lit lhs2TeX.sty.lit $(DISTDIR)
	$(INSTALL) -m 644 Makefile common.mk config.mk.in $(DISTDIR)
	$(INSTALL) -m 755 configure mkinstalldirs install-sh $(DISTDIR)
	$(INSTALL) -m 644 TODO LICENSE RELEASE $(DISTDIR)
	cat INSTALL | sed -e "s/@ProgramVersion@/$(PACKAGE_VERSION)/" \
		> $(DISTDIR)/INSTALL
	chmod 644 $(DISTDIR)/INSTALL
	cd doc; $(MAKE) srcdist
	$(INSTALL) -m 644 polytable/* $(DISTDIR)/polytable
	$(INSTALL) -m 644 Testsuite/*.{lhs,snip} Makefile $(DISTDIR)/Testsuite
	$(INSTALL) -m 644 Examples/*.lhs $(DISTDIR)/Examples
	$(INSTALL) -m 755 Examples/lhs2TeXpre $(DISTDIR)/Examples
	tar cvjf $(DISTDIR).tar.bzip2 $(DISTDIR)
	chmod 644 $(DISTDIR).tar.bzip2

backup:
	cd ..; \
	$(RM) -f Literate.tar Literate.tar.gz; \
	tar -cf Literate.tar Literate; \
	gzip Literate.tar; \
	chmod a+r Literate.tar.gz

clean :
#	clean
	$(RM) -f lhs2TeX $(sections) $(snips) $(objects) *.hi *.dvi *.ps
	-$(RM) -f *.d *.dd *.ld *.ldd
	$(RM) -f lhs2TeX.sty lhs2TeX.fmt
	$(RM) -f Lhs2TeX.tex lhs2TeX.sty.tex lhs2TeX.fmt.tex Makefile.tex 
	cd Guide; $(MAKE) clean

# all:
# 	$(MAKE) install
# 	$(MAKE) Lhs2TeX.dvi

include common.mk
