include ../config.mk

# use in-place lhs2TeX
LHS2TEX = ../lhs2TeX

SNIPPETSGD  = $(shell $(GREP) "\\\\input{" Guide2.lhs | $(SED) "s/^.*input{\(.*\)}.*$$/\1.tex/")
ifdef STC
SNIPPETSSTC = $(shell $(GREP) "\\\\input{" STC.lhs | $(SED) "s/^.*input{\(.*\)}.*$$/\1.tex/")
SNIPPETSTLL = $(shell $(GREP) "\\\\input{" TLL.lhs | $(SED) "s/^.*input{\(.*\)}.*$$/\1.tex/")
STCFLAGS="--let=stc=True"
endif

SNIPPETSIN    = $(wildcard *.lhs.in)
SNIPPETSLHS   = $(filter-out $(SNIPPETSIN:.in=), $(SNIPPETSGD:.tex=.lhs))

DONTBUILD = $(shell test -f Guide2.dontbuild && echo yes)

# default target
ifeq ($(DONTBUILD),yes)
all : dontbuildwarning
else
all : Guide2.pdf
endif

# in-place polytable.sty
polytable.sty :
	$(LN_S) ../polytable/polytable.sty .
	$(LN_S) ../polytable/lazylist.sty .

RawSearchPath.lhs : ../src/Version.lhs
	echo '\begin{code}' > $@
	$(LHS2TEX) --searchpath >> $@
	echo '\end{code}' >> $@

SearchPath.lhs : RawSearchPath.lhs
	touch $@

srcdist :
	$(INSTALL) -m 644 $(SNIPPETSIN) ../$(DISTDIR)/doc
	$(INSTALL) -m 644 $(SNIPPETSLHS) ../$(DISTDIR)/doc
	$(INSTALL) -m 644 RawSearchPath.lhs ../$(DISTDIR)/doc
	$(INSTALL) -m 644 $(wildcard *.fmt) ../$(DISTDIR)/doc
	$(INSTALL) -m 644 Guide2.lhs Guide2.pdf Makefile ../$(DISTDIR)/doc
	$(INSTALL) -m 755 lhs2TeXpre ../$(DISTDIR)/doc
	touch ../$(DISTDIR)/doc/Guide2.dontbuild

dontbuildwarning :
	echo "----------------------------------------------"; \
	echo "To rebuild Guide2.pdf, remove Guide2.dontbuild"; \
	echo "and call make again."; \
	echo "----------------------------------------------"; \

%.pdf : %.tex polytable.sty
	$(SHELL) -c ' \
		$(PDFLATEX) $(PDFLATEX_OPTS) $<; \
		while $(GREP) -c "Warning.*Rerun" $(<:.tex=.log); \
			do $(PDFLATEX) $(PDFLATEX_OPTS) $<; done;'; \

Guide2.tex : Guide2.lhs $(SNIPPETSGD)
	$(LHS2TEX) --poly -P.:..:../src: $< > $@

STC.tex : STC.lhs $(SNIPPETSSTC)
	$(LHS2TEX) --poly $< > $@

TLL.tex : TLL.lhs $(SNIPPETSTLL)
	$(LHS2TEX) --poly $< > $@

# determine mode and then run lhs2TeX
%.tex : %.lhs
	MODE=`cat $< \
		| $(GREP) "^%include" \
		| head -n 1 \
		| $(SED) -e "s/^%include \(.*\)\.fmt/\1/" \
		| $(SED) -e "s/verbatim/verb/" \
			 -e "s/stupid/math/" \
		      	 -e "s/tex/poly/" \
		      	 -e "s/polytt/poly/" \
		      	 -e "s/typewriter/tt/"` && \
	echo $${MODE} && \
	$(LHS2TEX) --$${MODE} $(STCFLAGS) $< > $@

.PHONY : all srcdist clean
clean :
	rm -f $(SNIPPETSGD) $(SNIPPETSSTC)

