%include verbatim.fmt

\begingroup
\let\origtt=\tt
\def\tt#1{\origtt}
%if style == newcode

module InteractivePre where

%endif

\begin{code}
%format SPL(x) = $ ( x )
%if style == newcode
%format QU(x)  = [ | x | ]
%format ^^     = " "
%else
%format QU(x)  = "\llbracket " x "\rrbracket "
%format ^^     = "\; "
%endif

%options ghci -fth -pgmL ../lhs2TeX -optL--pre

This is a rather stupid way of computing |42| using
Template Haskell:

> answer = SPL(foldr1 (\x y -> QU(SPL(x) + SPL(y))) (replicate 21 ^^ QU(2)))

The answer is indeed \eval{answer}.
\end{code}
\endgroup
