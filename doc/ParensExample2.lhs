%include poly.fmt

\begingroup
\setlength{\abovedisplayskip}{1pt}
\setlength{\belowdisplayskip}{1pt}

%format eval a = "\llbracket " a "\rrbracket "
\begin{code}
size (eval (2 + 2)) 
\end{code}
%format (eval (a)) = "\llbracket " a "\rrbracket "
\begin{code}
size (eval (2 + 2)) 
\end{code}

\endgroup
