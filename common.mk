
.SUFFIXES : .tex .dvi .pdf .ps

.tex.dvi:
	sh -c ' \
	  $(LATEX) $(LATEX_OPTS) $<; \
	  while grep -c "Warning.*Rerun" $(<:.tex=.log); \
	    do $(LATEX) $(LATEX_OPTS) $<; done;'

.tex.pdf:
	sh -c ' \
	  $(PDFLATEX) $(PDFLATEX_OPTS) $<; \
	  while grep -c "Warning.*Rerun" $(<:.tex=.log); \
	    do $(PDFLATEX) $(PDFLATEX_OPTS) $<; done;'

.dvi.ps:
	$(DVIPS) -D600 -o $@ $<
