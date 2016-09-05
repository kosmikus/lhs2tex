%include verbatim.fmt
\begingroup
\let\origtt=\ttfamily
\def\ttfamily#1#2{\origtt}

>%format alpha = "\alpha"
>
>\begin{code}
>tan alpha = sin alpha / cos alpha
>\end{code}

\endgroup
