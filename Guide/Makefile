
include ../config.mk

#-----------------------------------------------------------------------------
# directories and files

file		:= Guide
sources         := $(wildcard *.lhs)

#-----------------------------------------------------------------------------
# commands

LHS2TEX         := ../lhs2TeX
flags		:= --math --align=33

#-----------------------------------------------------------------------------
# pattern rules

%.tex : %.lhs $(sources)
	$(LHS2TEX) $(flags) $< > $@

%.ps : %.dvi
	$(DVIPS) -D600 -o $@ $<

#-----------------------------------------------------------------------------
# common rules

include ../common.mk

