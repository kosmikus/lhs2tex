%-------------------------------=  --------------------------------------------
\subsection{Finite maps}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> module FiniteMap              (  FiniteMap, empty, fromList, add, lookup, (!), keys)
> where
> import Prelude hiding         (  lookup  )
> import Data.Maybe

%endif

> type FiniteMap key val        =  Trie key val
>
> data Trie key val             =  Leaf [key] val
>                               |  Node [(key, Trie key val)] (Maybe val)

The value associated with the empty sequence is contained in the |Maybe b|
part. \NB |Node [('a', empty)] Nothing| is not legal.

> empty                         :: Trie key val
> empty                         =  Node [] Nothing
>
> fromList                      :: (Ord key) => [([key], val)] -> Trie key val
> fromList                      =  foldr add empty
>
> add                           :: (Ord a) => ([a], b) -> Trie a b -> Trie a b
> add (x, v) t                  =  insert t x v
>
> lookup                        :: (Ord a) => [a] -> Trie a b -> Maybe b
> lookup x (Leaf y w)
>     | x == y                  =  Just w
>     | otherwise               =  Nothing
> lookup [] (Node _ts w)        =  w
> lookup (a : x) (Node ts _w)   =  lookupList ts
>     where
>     lookupList []             =  Nothing
>     lookupList ((b, t) : ts') =  case compare b a of
>         LT                    -> lookupList ts'
>         EQ                    -> lookup x t
>         GT                    -> Nothing

> keys                          :: (Ord a) => Trie a b -> [[a]]
> keys                          =  keys' []
>     where
>     keys' acc (Leaf x _)      =  [acc ++ x]
>     keys' acc (Node xs v)     =  maybe [] (const [acc]) v ++
>                                  concatMap (\ (x,t) -> keys' (acc ++ [x]) t) xs

Derived functions.

> (!)                           :: (Ord a) => Trie a b -> [a] -> b
> t ! k                         =  fromJust (lookup k t)

Auxiliary functions.
%{
%align 41

> insert                                :: (Ord a) => Trie a b -> [a] -> b -> Trie a b
> insert (Leaf [] _) [] v               =  Leaf [] v
> insert (Leaf (b : y) w) [] v          =  Node [(b, Leaf y w)] (Just v)
> insert (Leaf [] w) (a : x) v          =  Node [(a, Leaf x v)] (Just w)
> insert (Leaf (b : y) w) (a : x) v     =  case compare b a of
>     LT                                -> Node [(b, Leaf y w), (a, Leaf x v)] Nothing
>     EQ                                -> Node [(a, insert (Leaf y w) x v)] Nothing
>     GT                                -> Node [(a, Leaf x v), (b, Leaf y w)] Nothing
> insert (Node ts _w) [] v              =  Node ts (Just v)
> insert (Node ts w)  (a : x) v         =  Node (insList ts) w
>     where
>     insList []                        =  [(a,Leaf x v)]
>     insList ((b, t) : ts')            =  case compare b a of
>         LT                            -> (b, t) : insList ts'
>         EQ                            -> (b, insert t x v) : ts'
>         GT                            -> (a, Leaf x v) : (b, t) : ts'

%}
