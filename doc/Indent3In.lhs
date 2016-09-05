%include verbatim.fmt
\begingroup
\let\origtt=\ttfamily
\def\ttfamily#1#2{\origtt \makebox[0pt]{\phantom{X}}}
>%format foo = verylongfoo
>\begin{code}
>test  1
>foo  bar
>      2
>\end{code}
\endgroup
