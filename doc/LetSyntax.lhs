%include tex.fmt
\newcommand*{\amp}{@&&@}%
%format && = "\text{\amp}"
%format || = "\text{\ttfamily ||}"
%format == = "\text{\ttfamily ==}"
%format /= = "\text{\ttfamily /=}"
%format <  = "\text{\ttfamily <}"
%format <= = "\text{\ttfamily <=}"
%format >= = "\text{\ttfamily >=}"
%format >  = "\text{\ttfamily >}"
%format ++ = "\text{\ttfamily ++}"
%format +  = "\text{\ttfamily +}"
%format -  = "\text{\ttfamily -}"
%format *  = "\text{\ttfamily *}"
%format /  = "\text{\ttfamily /}"

\begin{code}
dir(^let^) ^^ ent(varname) ^^ syn(=) ^^ ent(expression)

ent(expression)   ::=  ent(application) ^^ many(ent(operator) ^^ ent(application))
ent(application)  ::=  opt(term(not)) ^^ ent(atom)
ent(atom)         ::=  ent(varid) | term(True) | term(False) | ent(string') | ent(numeral) | (ent(expression))
ent(operator)     ::=  && | || | == | /= | < | <= | >= | > | ++ | + | - | * | /
\end{code}
