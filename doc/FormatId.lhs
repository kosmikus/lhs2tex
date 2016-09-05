%include verbatim.fmt
\begingroup
\let\origtt=\ttfamily
\def\ttfamily#1#2{\origtt}

>%format new      = "\mathbf{new}"
>%format text0    = text
>%format text_new = text "_{" new "}"

\endgroup
