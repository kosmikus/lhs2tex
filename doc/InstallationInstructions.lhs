%include typewriter.fmt
%subst code a = "\begin{colorverb}'n\texfamily " a "\end{colorverb}'n" 

\begingroup
\let\origtt=\texfamily
\def\texfamily{\origtt\makebox[0pt]{\phantom{X}}}
%format ProgramVersion = "\ProgramVersion "
\begin{code}
$ cd /somewhere/lhs2TeX-ProgramVersion
$ ./configure
$ make
$ make install
\end{code}
\endgroup
