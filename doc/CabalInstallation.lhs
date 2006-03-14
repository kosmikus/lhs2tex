%include typewriter.fmt
%subst code a = "\begin{colorverb}'n\texfamily " a "\end{colorverb}'n" 

\begingroup
\let\origtt=\texfamily
\def\texfamily{\origtt\makebox[0pt]{\phantom{X}}}
\begin{code}
$ runghc Setup configure
$ runghc Setup build
$ runghc Setup install
\end{code}
\endgroup
