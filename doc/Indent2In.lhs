%include verbatim.fmt
\begingroup
\let\origtt=\ttfamily
\def\ttfamily#1{\origtt \makebox[0pt]{\phantom{X}}}
\begin{code}
unionBy           ::  (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy eq xs ys  =   xs ++ foldl (flip (deleteBy eq))
                                  (nubBy eq ys)
\end{code}
\endgroup
