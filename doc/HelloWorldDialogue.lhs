%include verbatim.fmt
\begingroup
\let\origtt=\tt
\def\tt#1{\origtt}
\begin{code}
$ lhs2TeX --poly HelloWorld.lhs > HelloWorld.tex
$ pdflatex HelloWorld.tex
\end{code}
\endgroup
