%include poly.fmt

\begin{code}
unionBy           ::  (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy eq xs ys  =   xs ++ foldl (flip (deleteBy eq))
                                  (nubBy eq ys)
\end{code}
