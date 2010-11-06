%include typewriter.fmt
%subst code a = "\begin{colorverb}'n\texfamily " a "\end{colorverb}'n" 

\begingroup
\let\origtt=\texfamily
\def\texfamily{\origtt\makebox[0pt]{\phantom{X}}}
\begin{code}
$ cabal update
$ cabal install lhs2tex
\end{code}
\endgroup
