
bindir          = ${exec_prefix}/bin
datadir         = ${prefix}/share
stydir          = ${prefix}/share/lhs2tex
docdir          = ${prefix}/share/doc/lhs2tex-1.5
prefix          = /usr/local
exec_prefix     = ${prefix}

binpath		:= $(HOME)/bin

GHC		:= /usr/bin/ghc
# GHCFLAGS	:= -Wall -O -recomp -package lang
GHCFLAGS        := -O -package lang

INSTALL         := /bin/install -c
MV              := /bin/mv
CP              := /bin/cp
RM              := /bin/rm
MKDIR           := /bin/mkdir
TOUCH           := /bin/touch
DIFF            := /usr/bin/diff
FIND            := /usr/bin/find
LN_S            := ln -s
LATEX           := /opt/bin/latex
PDFLATEX        := /opt/bin/pdflatex
XDVI            := /opt/bin/xdvi
GV              := /usr/bin/gv
DVIPS           := /opt/bin/dvips
SED             := /bin/sed
GREP            := /bin/grep
SORT            := /usr/bin/sort
UNIQ            := /usr/bin/uniq
