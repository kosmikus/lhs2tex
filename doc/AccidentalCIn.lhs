%include typewriter.fmt
%subst code a = "\begin{colorverb}'n\texfamily " a "\end{colorverb}'n" 
%format \ = "\char''134"
%format let = "let"
%format in  = "in"
%format where = "where"
%format ->  = "->"
%format !=  = "{\origcolor{hcolor}" = "}"
%format !:: = "{\origcolor{hcolor}" :: "}"
%format ^ = " "
%format C = "{\origcolor{hcolor}"
%format D = "}"
\begingroup
\let\origtt=\texfamily
\let\small\footnotesize
\def\texfamily#1{\origtt}
>%format <| = "\lhd "
>
>> options  !::  [String] -> ([Class],[String])
>> options  !=   foldr (<|) ([],[])
>>   where   C^"-align"^D     <| (ds,s:  as) = (Dir Align    s :  ds,     as)
>>           C^('-':'i':s)^D <| (ds,    as) = (Dir Include  s :  ds,     as)
>>           C^('-':'l':s)^D <| (ds,    as) = (Dir Let      s :  ds,     as)
>>           C^s^D           <| (ds,    as) = (                  ds,s :  as)
\endgroup
