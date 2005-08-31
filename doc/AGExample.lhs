%include poly.fmt
%format ^  = " "
%format ^^ = "\;"
%format ATTR = "\mathbf{ATTR}"
%format SEM  = "\mathbf{SEM}"
%format lhs  = "\mathbf{lhs}"
%format .  = "."
%format *  = "\times"
%format (A(n)(f)) = @ n . f
\begin{code}
ATTR Expr Factor   [ ^^ | ^^ | numvars   : Int  ]
ATTR Expr Factor   [ ^^ | ^^ | value     : Int  ]

SEM Expr
  |  Sum       
              lhs   .  value     =  A left value    +  A right value
                    .  numvars   =  A left numvars  +  A right numvars
SEM Factor
  |  Prod      
              lhs   .  value     =  A left value    *  A right value
                    .  numvars   =  A left numvars  +  A right numvars
\end{code}
