%let array=True
%include stupid.fmt
%subst code a = "\begin{colorarray}{@{}lcl@{}}'n" a "\end{colorarray}'n" 

%align 28
\begin{code}
zip                        :: [a] -> [b] -> [(a,b)]
zip                        =  zipWith  (\a b -> (a,b))

zipWith                    :: (a->b->c) -> [a]->[b]->[c]
zipWith z (a:as) (b:bs)    =  z a b : zipWith z as bs
zipWith _ _      _         =  []

partition                  :: (a -> Bool) -> [a] -> ([a],[a])
partition p xs             =  foldr select ([],[]) xs
   where select x (ts,fs) | p x       = (x:ts,fs)
                          | otherwise = (ts,x:fs)
\end{code}


