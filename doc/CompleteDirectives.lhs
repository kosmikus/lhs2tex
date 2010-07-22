%include tex.fmt

\begingroup
\invisiblecomments
\def\bropen{@{@}
\def\brclos{@}@}
%{
%format bropen = "\bropen"
%format brclose = "\brclos"
\begin{code}
dir(include)              -- include a file
dir(format)               -- formatting directive for an identifer/operator
dir(bropen)               -- begin of an @lhs2TeX@ group
dir(brclose)              -- end of an @lhs2TeX@ group
dir(^let^)                -- set a toggle
dir(^if^)                 -- test a condition
dir(^else^)               -- second part of conditional
dir(elif)                 -- @else@ combined with @if@
dir(endif)                -- end of a conditional
dir(latency)              -- tweak alignment in \textbf{poly} mode
dir(separation)     ^^^   -- tweak alignment in \textbf{poly} mode
dir(align)                -- set alignment column in \textbf{math} mode
dir(options)              -- set options for call of external program
dir(subst)                -- primitive formatting directive
dir(file)                 -- set filename
\end{code}
%}
\endgroup
