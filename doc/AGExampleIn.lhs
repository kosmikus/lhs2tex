%include verbatim.fmt
\begingroup
\let\origtt=\tt
%\let\small\scriptsize
\def\tt#1#2{\origtt\makebox[0pt]{\phantom{X}}}

>%format ^  = " "
>%format ^^ = "\;"
>%format ATTR = "\mathbf{ATTR}"
>%format SEM  = "\mathbf{SEM}"
>%format lhs  = "\mathbf{lhs}"
>%format .  = "."
>%format *  = "\times "
>\begin{code}
>ATTR Expr Factor  [ ^^ | ^^ | numvars   : Int  ]
>ATTR Expr Factor  [ ^^ | ^^ | value     : Int  ]
>
>SEM Expr
>  |  Sum       
>              lhs   .  value     =  @left.value    +  @right.value
>                    .  numvars   =  @left.numvars  +  @right.numvars
>SEM Factor
>  |  Prod      
>              lhs   .  value     =  @left.value    *  @right.value
>                    .  numvars   =  @left.numvars  +  @right.numvars
>\end{code}

\endgroup
