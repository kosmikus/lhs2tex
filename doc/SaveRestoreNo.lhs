%include poly.fmt

\begingroup

\begin{code}
intersperse               ::  a -> [a] -> [a]
intersperse  _    []      =   []
intersperse  _    [x]     =   [x]
\end{code}
The only really interesting case is the one for lists 
containing at least two elements:
\begin{code}
intersperse  sep  (x:xs)  =   x : sep : intersperse sep xs
\end{code}
\endgroup
