%include verbatim.fmt

\begingroup
\let\origtt=\ttfamily
\def\ttfamily#1{\origtt}
\begin{code}
%format ^  = " "
%format ^^ = "\;"
\end{code}
\endgroup
