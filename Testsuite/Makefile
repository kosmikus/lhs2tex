
include ../config.mk

#-----------------------------------------------------------------------------
# directories and files

snips		:= $(wildcard *.snip)
texs       	:= $(foreach file, $(snips:.snip=.tex), $(file))

LHS2TEX = ../lhs2TeX

#-----------------------------------------------------------------------------
# pattern rules

%.tex : %.snip
#	lhs2TeX -verb -i../lhs2TeX.fmt -l'meta = True' $< > $@
	$(LHS2TEX) --verbose --math --align=33 -smeta -i../lhs2TeX.fmt $< > $@

%.tex : %.lhs
#	lhs2TeX -verb -i../lhs2TeX.fmt -l'meta = True' $< > $@
	$(LHS2TEX) --verbose --math --align=33 -smeta -i../lhs2TeX.fmt $< > $@

%.ps : %.dvi
	$(DVIPS) -D600 -o $@ $<

#-----------------------------------------------------------------------------

.PHONY: show view print clean

Test.dvi : $(texs)

show : Test.dvi
	$(XDVI) -s 3 Test.dvi &

view : Test.ps
	xe ghostview Test.ps &

print : Test.dvi
	$(DVIPS) -D600 -f Test.dvi | lpr -Pa -Zl

clean :
	rm -f Test.tex $(texs) *.dvi *.ps

include ../common.mk
