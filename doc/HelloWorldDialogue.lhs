%include verbatim.fmt
\begingroup
\let\origtt=\ttfamily
\def\ttfamily#1{\origtt}
\begin{code}
$ lhs2TeX -o HelloWorld.tex HelloWorld.lhs
$ pdflatex HelloWorld.tex
\end{code}
\endgroup
