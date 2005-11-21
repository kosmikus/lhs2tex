\documentclass{article}

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include spacing.fmt

\begin{document}

It is irrelevant how typ is formatted, but it must be a parametrized
format for the bug to be triggered.

%format typ(a) = a

This is a @lhs2TeX@ bug. The double colons are not aligned, although
they should be.

\begin{code}
map    ^^  typ( a1 :: *, a2 :: * )                             ::  (map ^^ typ(a1,a2)) => a1 -> a2
zipWith^^  typ( a1 :: *, a2 :: *, a3 :: * )                    ::  (zipWith ^^ typ(a1,a2,a3)) => a1 -> a2 -> a3 
collect^^  typ( a :: * | b :: * )                              ::  (collect ^^ typ(a | b)) => a -> [c]
equal  ^^  typ( a :: * )                                       ::  (  enum ^^ typ(a), equal ^^ typ(a)) 
                                                                          => a -> a -> Bool ^^.
\end{code}

This is a workaround:

\begin{code}
map    ^^  typ( a1 :: *, a2 :: * )              ^              ::  (map ^^ typ(a1,a2)) => a1 -> a2
zipWith^^  typ( a1 :: *, a2 :: *, a3 :: * )     ^              ::  (zipWith ^^ typ(a1,a2,a3)) => a1 -> a2 -> a3 
collect^^  typ( a :: * | b :: * )               ^              ::  (collect ^^ typ(a | b)) => a -> [c]
equal  ^^  typ( a :: * )                        ^              ::  (  enum ^^ typ(a), equal ^^ typ(a)) 
                                                                          => a -> a -> Bool ^^.
\end{code}

\end{document}
