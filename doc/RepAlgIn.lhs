%include typewriter.fmt
%subst code a = "\begin{colorverb}'n\texfamily " a "\end{colorverb}'n" 
%format \ = "\char''134"
%format let = "let"
%format in  = "in"
%format ->  = "->"
%format !=  = "{\origcolor{hcolor}" = "}"
%format llt = "{\origcolor{hcolor}" lt "}"
\begingroup
\let\origtt=\texfamily
\let\small\footnotesize
\def\texfamily#1{\origtt}
>> rep_alg         =  (\  _          -> \m ->  Leaf m
>>                    ,\  lfun rfun  -> \m ->  let  lt  != lfun m
>>                                                  rt  != rfun m
>>                                             in   Bin llt rt
>>                    )
>> replace_min' t  =  (cata_Tree rep_alg t) (cata_Tree min_alg t)
\endgroup
