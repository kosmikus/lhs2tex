module Lhs2TeX.Utils
  (module Lhs2TeX.Utils, module Data.ListTrie.Map)
  where

import Data.Char
import Data.Map
import Data.ListTrie.Map
import System.IO
import System.Exit

type LineNumber = Int

-- | Anything can be explicitly tagged with a line number.
data Numbered a = Numbered !LineNumber a

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

-- | Find, within a limit, a position in a string that fulfills
-- a certain predicate. Returns the prefix and the remaining string
-- where the predicate holds for the remaining string.
match :: Int -> ([a] -> Bool) -> [a] -> Maybe ([a], [a])
match n pred [] = return ([], [])
match n pred xs@(s : ss)
  | pred xs     = return ([], xs)
  | n == 0      = Nothing
  | otherwise   = do
                    rec <- match (n - 1) pred ss
                    return (s <| rec)

-- | Delete up to one leading and trailing blank line.
trim :: String -> String
trim = reverse . skip . reverse . skip
  where
    skip :: String -> String
    skip txt
      | all isSpace xs = ys
      | otherwise      = txt
      where
        (xs, ys) = breakAfter (== '\n') txt

-- | Type of tries that is used for most lookup tables in lhs2TeX.
type Trie a = TrieMap Map Char a

-- | Proxy argument when the type checker needs additional guidance.
data Proxy a = Proxy
