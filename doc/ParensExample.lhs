%include poly.fmt

\begingroup
\setlength{\abovedisplayskip}{1pt}
\setlength{\belowdisplayskip}{1pt}

%format ^^                = "\;"
%format (ptest (a) b (c)) = ptest ^^ a ^^ b ^^ c
\begin{code}
ptest a b c
(ptest (a) (b) (c))
((ptest((a)) ((b)) ((c))))
\end{code}

\endgroup
