%include lhs2TeX.fmt

\begin{code}
zip                     :: [a] -> [b] -> [(a,b)]
zip                     =  zipWith  (\a b -> (a,b))

zipWith                 :: (a->b->c) -> [a]->[b]->[c]
zipWith z (a:as) (b:bs) =  z a b : zipWith z as bs
zipWith _ _      _      =  []
\end{code}


