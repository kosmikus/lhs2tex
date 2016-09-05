%include verbatim.fmt
\begingroup
\let\origtt=\ttfamily
\def\ttfamily#1#2{\origtt}
>%format abs (a) = "\mathopen{|}" a "\mathclose{|}"
>%format ~>      = "\leadsto"
>The |abs| function computes the absolute value of 
>an integer:
>\begin{code}
>abs(-2) ~> 2
>\end{code}
\endgroup
