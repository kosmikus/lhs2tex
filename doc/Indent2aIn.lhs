%include typewriter.fmt
%subst code a = "\begin{colorverb}'n\texfamily " a "\end{colorverb}'n" 
%format \ = "\char''134"
%format let = "let"
%format in  = "in"
%format case = "case"
%format of = "of"
%format ->  = "->"
%format { = "{\origcolor{hcolor}" ( "}"
%format } = )
\begingroup
\let\origtt=\texfamily
\let\small\footnotesize
\def\texfamily{\origtt\makebox[0pt]{\phantom{X}}}
\begin{code}
scanr1        ::  (a -> a -> a) -> [a] -> [a]
scanr1 f xxs  =   case xxs of
                     x:xs ->  case xs of
                                 []  ->  [x]
                                 _   ->  let 
                                            qs = scanr1 f xs 
                                         in
                                            case qs of 
                                               q:_ -> f x q : qs

\end{code}
\endgroup
