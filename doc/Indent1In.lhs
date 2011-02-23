%include typewriter.fmt
%subst code a = "\begin{colorverb}'n\texfamily " a "\end{colorverb}'n" 
%format \ = "\char''134"
%format let = "let"
%format in  = "in"
%format ->  = "->"
%format { = "{\origcolor{hcolor}" ( "}"
%format } = )
\begingroup
\let\origtt=\texfamily
\let\small\footnotesize
\def\texfamily{\origtt\makebox[0pt]{\phantom{X}}}
\begin{code}
> unionBy           ::  (a -> a -> Bool) -> [a] -> [a] -> [a]
> unionBy eq xs ys  =   xs ++ foldl  {flip (deleteBy eq)}
>                                    {nubBy eq ys}
\end{code}
\endgroup
