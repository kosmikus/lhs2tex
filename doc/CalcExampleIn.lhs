%include verbatim.fmt
\begingroup
\let\origtt=\ttfamily
%if stc
\let\small\scriptsize
%endif
\def\ttfamily#1#2{\origtt\makebox[0pt]{\phantom{X}}}

>\def\commentbegin{\quad\{\ }
>\def\commentend{\}}
>\begin{spec}
>    map (+1) [1,2,3]
>
>==  {- desugaring of |(:)| -}
>
>    map (+1) (1 : [2,3])
>
>==  {- definition of |map| -}
>
>    (+1) 1  :  map (+1) [2,3]
>
>==  {- performing the addition on the head -}
>
>    2       :  map (+1) [2,3]
>
>==  {- recursive application of |map| -}
>
>    2       :  [3,4]
>
>==  {- list syntactic sugar -}
>
>    [2,3,4]
>\end{spec}

\endgroup
