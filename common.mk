
# .SUFFIXES : .tex .dvi .pdf .ps

%.dvi : %.tex
	sh -c ' \
	  $(LATEX) $(LATEX_OPTS) $<; \
	  while grep -c "Warning.*Rerun" $(<:.tex=.log); \
	    do $(LATEX) $(LATEX_OPTS) $<; done;'

%.pdf : %.tex
	sh -c ' \
	  $(PDFLATEX) $(PDFLATEX_OPTS) $<; \
	  while grep -c "Warning.*Rerun" $(<:.tex=.log); \
	    do $(PDFLATEX) $(PDFLATEX_OPTS) $<; done;'

%.ps : %.dvi
	$(DVIPS) -D600 -o $@ $<
