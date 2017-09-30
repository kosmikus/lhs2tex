%-------------------------------=  --------------------------------------------
\subsection{Auxiliaries}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> module Auxiliaries            (  module Auxiliaries  )
> where
>
> import Data.Char              (  isSpace  )
> import Control.Arrow          (  (>>>), Kleisli(..) )
> import qualified Control.Arrow as A
> import Control.Monad          (  MonadPlus(..)  )
> import Control.Monad.Error

%endif

> infixr 5 {-"\;"-} <|  -- same fixity as `|:|'

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Operations on chars}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> unNL                          :: Char -> Char
> unNL '\n'                     =  ' '
> unNL c                        =  c

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Operations on lists}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> splitOn                       :: (Char -> Bool) -> String -> [String]
> splitOn p s                   =  case dropWhile p s of
>                                    "" -> []
>                                    s' -> w : splitOn p s''
>                                            where (w,s'') = break p s'

> rtake                         :: Int -> [a] -> [a]
> rtake n                       =  reverse . take n . reverse

> inverse                       :: [(a, b)] -> [(b, a)]
> inverse bs                    =  [ (b, a) | (a, b) <- bs ]

> merge                         :: (Ord a) => [a] -> [a] -> [a]
> merge [] bs                   =  bs
> merge as@(a : _) []           =  as
> merge as@(a : as')  bs@(b : bs')
>     | a <= b                  =  a : merge as' bs
>     | otherwise               =  b : merge as  bs'

%{
%format (sub (a) (b)) = "{" a "}_{" b "}"
The call |breakAfter p [sub a 1,..,sub a n]| yields |([sub a 1,..,sub a
i], [sub a (i+1),..,sub a n])| such that |p (sub a i) = True| and |p
(sub a j) = False| for |j < i|.
%}

> breakAfter                    :: (a -> Bool) -> [a] -> ([a], [a])
> breakAfter p []               =  ([], [])
> breakAfter p (a : as)
>     | p a                     =  ([a], as)
>     | otherwise               =  a <| breakAfter p as

> breaks                        :: ([a] -> Bool) -> [a] -> ([a], [a])
> breaks p []                   =  ([], [])
> breaks p as@(a : as')
>     | p as                    =  ([], as)
>     | otherwise               =  a <| breaks p as'

> -- isPrefix                      :: (Eq a) => [a] -> [a] -> Bool
> -- p `isPrefix` as               =  p == take (length p) as

> withoutSpaces                 :: String -> String
> withoutSpaces s               =  filter (not . isSpace) s

> group                         :: Int -> [a] -> [[a]]
> group n                       =  repSplit (repeat n) >>> takeWhile (not . null)

> repSplit                      :: [Int] -> [a] -> [[a]]
> repSplit [] xs                =  []
> repSplit (n : ns) xs          =  ys : repSplit ns zs
>   where (ys, zs)              =  splitAt n xs

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Monad utilities}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> lift                          :: (Monad m) => (a -> b) -> (a -> m b)
> lift f a                      =  return (f a)

> (***)                         :: (Monad m) => (a -> m a') -> (b -> m b') -> (a, b) -> m (a', b')
> f *** g                       =  runKleisli (Kleisli f A.*** Kleisli g)

> fromRight                     :: Either a b -> b
> fromRight (Left _)            =  error "fromRight"
> fromRight (Right b)           =  b

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Miscellaneous}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

Some useful type abbreviations.

> type LineNo                   =  Int
> type Message                  =  String
> type Exc                      =  (Message, String)

> instance Error Exc where
>   noMsg    = ("unknown error", "")
>   strMsg s = (s, "")

Reverse Composition.

> (<|)                          :: a -> ([a], b) -> ([a], b)
> a <| (as, b)                  =  (a : as, b)
>
> impossible                    :: String -> a
> impossible name               =  error ("The `impossible' happened in \""
>                                         ++ name ++ "\"")

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Obsolete code}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> command                       :: String -> String -> String
> command name arg              =  "\\" ++ name ++ "{" ++ arg ++ "}"
>
> environment                   :: String -> String -> String
> environment name m            =  "\\begin{" ++ name ++ "}" ++ m
>                               ++ "\\end{" ++ name ++ "}"
