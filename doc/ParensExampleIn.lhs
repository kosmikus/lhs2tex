%include verbatim.fmt
\begingroup
\let\origtt=\tt
\def\tt#1#2{\origtt}
>%format ^^                = "\;"
>%format (ptest (a) b (c)) = ptest ^^ a ^^ b ^^ c
>\begin{code}
>ptest a b c
>(ptest (a) (b) (c))
>((ptest((a)) ((b)) ((c))))
>\end{code}
\endgroup
