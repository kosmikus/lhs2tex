%include verbatim.fmt

\begingroup
\let\origtt=\ttfamily
\def\ttfamily#1{\origtt}
\begin{code}
%if style == newcode
%format ^  =
%format ^^ = " "
%else
%format ^  = " "
%format ^^ = "\;"
%endif
\end{code}
\endgroup
