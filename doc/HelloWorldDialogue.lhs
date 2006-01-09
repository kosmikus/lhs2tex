%include verbatim.fmt
\begingroup
\let\origtt=\tt
\def\tt#1{\origtt}
\begin{code}
$ lhs2TeX -o HelloWorld.tex HelloWorld.lhs
$ pdflatex HelloWorld.tex
\end{code}
\endgroup
