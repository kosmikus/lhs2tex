#-----------------------------------------------------------------------------
# directories and files

snips		:= $(wildcard *.snip)
texs       	:= $(foreach file, $(snips:.snip=.tex), $(file))

#-----------------------------------------------------------------------------
# pattern rules

%.tex : %.snip
#	lhs2TeX -verb -i../lhs2TeX.fmt -l'meta = True' $< > $@
	lhs2TeX -math -align 33 -l'meta = True' -i../lhs2TeX.fmt $< > $@

%.tex : %.lhs
#	lhs2TeX -verb -i../lhs2TeX.fmt -l'meta = True' $< > $@
	lhs2TeX -math -align 33 -l'meta = True' -i../lhs2TeX.fmt $< > $@

%.ps : %.dvi
	dvips -D600 -o $@ $<

#-----------------------------------------------------------------------------

.PHONY: show view print clean

Test.dvi : $(texs) Test.tex
	latex Test

show : Test.dvi
	xdvi -s 3 Test.dvi &

view : Test.ps
	xe ghostview Test.ps &

print : Test.dvi
	dvips -D600 -f Test.dvi | lpr -Pa -Zl

clean :
	clean
	rm -f Test.tex $(texs) *.dvi *.ps
