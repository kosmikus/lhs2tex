\documentclass{article}

\usepackage{ifthen}

%include lineno.fmt
\arrayhs

\begin{document}

\title{Line numbering with lhs2\TeX}
\author{Andres L\"oh}
\maketitle

\noindent
This is a demo. Look at the lhs2\TeX\ sources to see how each of the examples is produced.

Let's start with an example having numbers every five lines.
\numberstep{5}%
\begin{code}
zipWithM            ::  Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c] 
zipWithM   f xs ys  =   sequence (zipWith f xs ys)

zipWithM_           ::  Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_  f xs ys  =   sequence_ (zipWith f xs ys)

foldM               ::  Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM f a []        =   return a
foldM f a (x : xs)  =   f a x >>= \ y -> foldM f y xs

filterM             ::  Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p []        =   return []
filterM p (x : xs)  =   do
                           b   <- p x
                           ys  <- filterM p xs
                           return (if b then (x : ys) else ys)
\end{code}
Normally, the numbering will just continue in the next code block:
\begin{code}
liftM    ::  Monad m => (a -> b) -> (m a -> m b)
liftM f  =   \ a ->  do
                        a' <- a; return (f a')
\end{code}
But one can also change the behaviour, for instance, let every
line be numbered:
\numberstep{1}%
\begin{code}
msum        ::  MonadPlus m => [m a] -> m a
msum        =   foldr mplus mzero

join        ::  Monad m => m (m a) -> m a
join x      =   x >>= id

when        ::  Monad m => Bool -> m () -> m ()
when p s    =   if p then s else return ()

unless      ::  Monad m => Bool -> m () -> m ()
unless p s  =   when (not p) s

ap          ::  Monad m => m (a -> b) -> m a -> m b
ap          =   liftM2 ($)
\end{code}
Or reset the numbering:
\numbersreset
\begin{code}
intersperse               ::  a -> [a] -> [a]
intersperse sep []        =   []
intersperse sep [x]       =   [x]
intersperse sep (x : xs)  =   x : sep : intersperse sep xs
\end{code}
Or turn the numbers off completely:
\numbersoff
\begin{code}
addListToFM_C combiner fm key_elt_pairs
  = foldl add fm key_elt_pairs  -- |foldl| adds from the left
  where
    add fmap (key,elt) = addToFM_C combiner fmap key elt
\end{code}
One can also change the formatting of the line numbers
in several ways (which is not necessarily an improvement, though):
\numbersreset
\numberwidth{1em}%
\numberson
\begingroup
\setlength\mathindent{0pt}
\renewcommand*{\formatlinenum}[1]{\makebox[0pt][r]{\makebox[2em][l]{\small\textsf{#1:}}}}%
\begin{code}
instance Show a => Show (Tree a) where
  show           = showTree
  showList ts s  = showForest ts ++ s

showTree    ::  Show a => Tree a -> String
showTree    =   drawTree . mapTree show

showForest  ::  Show a => Forest a -> String
showForest  =   unlines . map showTree

drawTree    ::  Tree String -> String
drawTree    =   unlines . draw

draw               ::  Tree String -> [String]
draw (Node x ts0)  =   grp this (space (length this)) (stLoop ts0)
 where 
       this           =  s1 ++ x ++ " "

       space n        =  replicate n ' '

       stLoop []      =  [""]
       stLoop [t]     =  grp s2 "  " (draw t)
       stLoop (t:ts)  =  grp s3 s4 (draw t) ++ [s4] ++ rsLoop ts

       rsLoop []      =  error "rsLoop:Unexpected empty list."
       rsLoop [t]     =  grp s5 "  " (draw t)
       rsLoop (t:ts)  =  grp s6 s4 (draw t) ++ [s4] ++ rsLoop ts

       grp fst0 rst   =  zipWith (++) (fst0:repeat rst)

       [s1,s2,s3,s4,s5,s6]  
                      =  ["- ", "--", "-+", " |", " `", " +"]
\end{code}
\endgroup
You can also have numbers on the right \dots
\numbersright
\begin{code}
back                 ::  Graph -> Table Int -> Graph
back g post          =   mapT select g
  where select v ws  =   [ w | w <- ws, post!v < post!w ]

cross                ::  Graph -> Table Int -> Table Int -> Graph
cross g pre post     =   mapT select g
  where select v ws  =   [ w | w <- ws, post!v > post!w, pre!v > pre!w ]

forward              ::  Graph -> Graph -> Table Int -> Graph
forward g tree pre   =   mapT select g
  where select v ws  =   [ w | w <- ws, pre!v < pre!w ] \\ tree!v
\end{code}

\end{document}
