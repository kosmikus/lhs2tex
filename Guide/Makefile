
include ../config.mk

#-----------------------------------------------------------------------------
# directories and files

file		:= Guide
sources         := $(wildcard *.lhs)
snips           := sorts.math spec.math block.math

#-----------------------------------------------------------------------------
# commands

LHS2TEX         := ../lhs2TeX
flags		:= --math --align=33 --verbose

#-----------------------------------------------------------------------------
# pattern rules

.PHONY : default clean

default : Guide.pdf

%.math : %.snip
	$(LHS2TEX) $(flags) -smeta -P.. -ilhs2TeX.fmt $< > $@

%.tex : %.lhs $(sources) $(snips)
	$(LHS2TEX) $(flags) -P.:.. $< > $@

clean:
	$(RM) -f *.log *.aux *.math Guide.tex

#-----------------------------------------------------------------------------
# common rules

include ../common.mk

