%include tex.fmt
\newcommand*{\bslash}{@\@}
\newcommand*{\atsym}{\texttt{\char64}}
%format \ = "\bslash "
%format @ = "\atsym "
%format @@ = @ @
%format > = "\texttt{>}"
%format < = "\texttt{<}"
%format columnspec = "\Varid{column\text{\itshape -}specifier}"

\invisiblecomments
\begin{code}
term(\aligncolumn){ent(integer)}{ent(columnspec)}
\end{code}
The |ent(integer)| denotes the number (i.e.~as displayed by the
editor) of a column. Note that @lhs2TeX@ starts counting columns at 1.
As |ent(columnspec)| one can use about the same strings that one
can use to format a column in a @tabular@ environment using the
\LaTeX\ @array@ (LINK!!) package. Table~\ref{columnspec} has a short 
(and not necessarily complete) overview.
\begin{table}
\centering
\begin{colorsurround}
\begin{tabularx}{\linewidth}{cX}
@l@ & left-align column \\
@c@ & center column \\
@r@ & right-align column \\
|term(p){ent(dimen)}| & make column of fixed width |ent(dimen)| \\
|@@{ent(tex)}| & can be used before or after the letter specifying
                 alignment to suppress inter-column space and typeset
                 |ent(tex)| instead; note that this is usually achieved
                 using just one \texttt{\atsym}, but as @lhs2TeX@ 
                 interprets the \texttt{\atsym}, it must be escaped \\
|>{ent(tex)}| & can be used before the letter specifying the alignment
              to insert |ent(tex)| directly in front of the entry
              of the column \\
|<{ent(tex)}| & can be used after the letter specifying the alignment
              to insert |ent(tex)| directly after the entry of the
              column
\end{tabularx}
\end{colorsurround}
\caption{Column specifiers for @\aligncolumn@}\label{columnspec}
\end{table}
