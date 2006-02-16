
include config.mk

psources        := Main.lhs TeXCommands.lhs TeXParser.lhs \
		   Typewriter.lhs Math.lhs MathPoly.lhs \
                   MathCommon.lhs NewCode.lhs \
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

ifdef SORT
ifdef UNIQ

MKLHSDEPEND = $(GREP) "^%include " $< \
               | $(SED) -e 's,^%include ,$*.tex : ,' \
               | $(SORT) | $(UNIQ) > $*.ld

MKFMTDEPEND = $(GREP) "^%include " $< \
               | $(SED) -e 's,^%include ,$*.fmt : ,' \
               | $(SORT) | $(UNIQ) > $*.ld

endif
endif

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

ifdef MKLHSDEPEND

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

-include $(sources:%.lhs=%.ld)

endif

%.tex : %.lhs lhs2TeX Lhs2TeX.fmt lhs2TeX.fmt
#	lhs2TeX -verb -iLhs2TeX.fmt $< > $@
	./lhs2TeX --math --align 33 -iLhs2TeX.fmt $< > $@

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

lhs2TeX : $(sources)
	$(GHC) $(GHCFLAGS) --make -o lhs2TeX $(sources)

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
	$(INSTALL) -m 644 Library/*.fmt $(DESTDIR)$(stydir)
	$(MKINSTDIR) $(DESTDIR)$(docdir)
	$(INSTALL) -m 644 doc/Guide2.pdf $(DESTDIR)$(docdir)
	$(MKINSTDIR) $(DESTDIR)$(mandir)/man1
	$(INSTALL) -m 644 lhs2TeX.1 $(DESTDIR)$(mandir)/man1
ifeq ($(INSTALL_POLYTABLE),yes)
# install polytable package
	$(MKINSTDIR) $(DESTDIR)$(polydir)
	$(INSTALL) -m 644 polytable/*.sty $(DESTDIR)$(polydir)
endif
	# $(MKINSTDIR) $(DESTDIR)$(texdir)
	# $(INSTALL) -m 644 Library/*.sty $(DESTDIR)$(texdir)
ifndef DESTDIR
	$(MKTEXLSR)
else
	echo "Please update the TeX filename database."
endif

srcdist : doc
	if test -d $(DISTDIR); then $(RM) -rf $(DISTDIR); fi
	$(MKINSTDIR) $(DISTDIR)
	$(MKINSTDIR) $(DISTDIR)/doc
	$(MKINSTDIR) $(DISTDIR)/polytable
	$(MKINSTDIR) $(DISTDIR)/Testsuite
	$(MKINSTDIR) $(DISTDIR)/Examples
	$(MKINSTDIR) $(DISTDIR)/Library
	$(INSTALL) -m 644 $(psources) Version.lhs.in $(snipssrc) $(DISTDIR)
	$(INSTALL) -m 644 Setup.hs lhs2tex.cabal $(DISTDIR)
	$(INSTALL) -m 644 lhs2TeX.fmt.lit lhs2TeX.sty.lit $(DISTDIR)
	$(INSTALL) -m 644 Makefile common.mk config.mk.in $(DISTDIR)
	$(INSTALL) -m 644 lhs2TeX.1.in $(DISTDIR)
	$(INSTALL) -m 755 configure mkinstalldirs install-sh $(DISTDIR)
	$(INSTALL) -m 644 TODO LICENSE RELEASE $(DISTDIR)
	cat INSTALL | sed -e "s/@ProgramVersion@/$(PACKAGE_VERSION)/" \
		> $(DISTDIR)/INSTALL
	chmod 644 $(DISTDIR)/INSTALL
	cd doc; $(MAKE) srcdist
	$(INSTALL) -m 644 polytable/*.{sty,pdf} $(DISTDIR)/polytable
	$(INSTALL) -m 644 Testsuite/*.{lhs,snip} Makefile $(DISTDIR)/Testsuite
	$(INSTALL) -m 644 Examples/*.lhs $(DISTDIR)/Examples
	$(INSTALL) -m 755 Examples/lhs2TeXpre $(DISTDIR)/Examples
	$(INSTALL) -m 644 Library/*.fmt $(DISTDIR)/Library
	tar cvjf $(DISTDIR).tar.bz2 $(DISTDIR)
	chmod 644 $(DISTDIR).tar.bz2

ifdef DISTTYPE

bindist: lhs2TeX lhs2TeX.fmt lhs2TeX.sty doc
	if test -d $(DISTDIR); then $(RM) -rf $(DISTDIR); fi
	$(MKINSTDIR) $(DISTDIR)
	$(MKINSTDIR) $(DISTDIR)/doc
	$(MKINSTDIR) $(DISTDIR)/polytable
	$(MKINSTDIR) $(DISTDIR)/Testsuite
	$(MKINSTDIR) $(DISTDIR)/Examples
	$(MKINSTDIR) $(DISTDIR)/Library
	$(INSTALL) -m 755 lhs2TeX $(DISTDIR)
	$(INSTALL) -m 644 lhs2TeX.fmt lhs2TeX.sty $(DISTDIR)
	$(INSTALL) -m 644 $(psources) Version.lhs.in $(snipssrc) $(DISTDIR)
	$(INSTALL) -m 644 lhs2TeX.fmt.lit lhs2TeX.sty.lit $(DISTDIR)
	$(INSTALL) -m 644 Makefile common.mk config.mk.in $(DISTDIR)
	$(INSTALL) -m 644 lhs2TeX.1.in $(DISTDIR)
	$(INSTALL) -m 755 configure mkinstalldirs install-sh $(DISTDIR)
	$(INSTALL) -m 644 TODO LICENSE RELEASE $(DISTDIR)
	cat INSTALL | sed -e "s/@ProgramVersion@/$(PACKAGE_VERSION)/" \
		> $(DISTDIR)/INSTALL
	chmod 644 $(DISTDIR)/INSTALL
	cd doc; $(MAKE) srcdist
	$(INSTALL) -m 644 polytable/*.{sty,pdf} $(DISTDIR)/polytable
	$(INSTALL) -m 644 Testsuite/*.{lhs,snip} Makefile $(DISTDIR)/Testsuite
	$(INSTALL) -m 644 Examples/*.lhs $(DISTDIR)/Examples
	$(INSTALL) -m 755 Examples/lhs2TeXpre $(DISTDIR)/Examples
	$(INSTALL) -m 644 Library/*.fmt $(DISTDIR)/Library
	tar cvjf $(DISTDIR)-$(DISTTYPE).tar.bz2 $(DISTDIR)
	chmod 644 $(DISTDIR)-$(DISTTYPE).tar.bz2

else

bindist:
	@echo "You must define DISTTYPE."

endif

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
	cd doc; $(MAKE) clean

# all:
# 	$(MAKE) install
# 	$(MAKE) Lhs2TeX.dvi

include common.mk
