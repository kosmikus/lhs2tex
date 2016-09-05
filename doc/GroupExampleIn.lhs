%include verbatim.fmt
\begingroup
\let\origtt=\ttfamily
\def\ttfamily#1{\origtt}
\begin{code}
In the beginning: |one|.\par
%format one = "\mathsf{1}"
Before the group: |one|.\par
%{
%format one = "\mathsf{one}"
Inside the group: |one|.\par
%}
After the group: |one|.
\end{code}
\endgroup
