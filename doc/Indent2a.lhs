%include poly.fmt

\begin{code}
scanr1        ::  (a -> a -> a) -> [a] -> [a]
scanr1 f xxs  =   case xxs of
                     x:xs ->  case xs of
                                 []  ->  [x]
                                 _   ->  let 
                                            qs = scanr1 f xs 
                                         in
                                            case qs of 
                                               q:_ -> f x q : qs

\end{code}
