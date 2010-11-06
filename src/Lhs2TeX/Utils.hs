module Lhs2TeX.Utils where

import System.IO
import System.Exit

type LineNo = Int

infixr 5 <|  -- same fixity as cons

-- | Conses an item to the first element of a pair.
(<|) :: a -> ([a], b) -> ([a], b)
a <| (as, b) = (a : as, b)

-- | Similar to `break', but takes a predicate on the entire
-- remaining string.
breaks :: ([a] -> Bool) -> [a] -> ([a], [a])
breaks p []            =  ([], [])
breaks p as@(a : as')
  | p as               =  ([], as)
  | otherwise          =  a <| breaks p as'

-- | Similar to `break', but it places the element that triggers
-- the break in the first list.
breakAfter :: (a -> Bool) -> [a] -> ([a], [a])
breakAfter p []        =  ([], [])
breakAfter p (a : as)
  | p a                =  ([a], as)
  | otherwise          =  a <| breakAfter p as
