-- | Pseudo-parser for TeX
module Lhs2TeX.TeX.Parser where

import Data.Char
import Data.List as List

import Lhs2TeX.Utils
import Lhs2TeX.Exception
import Lhs2TeX.TeX.Syntax
import Lhs2TeX.Representation

-- | Produces a stream of TeX tokens from an input string.
-- The TeX tokens retain all whitespace in the original file,
-- so that we can properly keep track of line numbers.
parse :: LineNumber -> String -> [Numbered Class]
parse n = number n . compress . classifyLine ""

-- | Used at the beginning of a line (or in particular, at the
-- beginning of a file). The function recognizes bird and inverse
-- bird tracks. These are turned into environments on the fly.
-- The first argument of 'classifyLine' is supposed to be either
-- an empty string or a newline character. Preceding newline
-- characters are put into the code section such that blank lines
-- before code sections are suppressed in the LaTeX document, but
-- still kept around for proper line number counting.
classifyLine :: String -> String -> [Class]
classifyLine _ []        = []
classifyLine n ('>' : s) = Environment Code (n ++ ' ' : t) :
                           classifyLine "" u
  where (t, u) = unbird '>' s
classifyLine n ('<' : s) = Environment Spec (n ++ ' ' : t) :
                           classifyLine "" u
  where (t, u) = unbird '<' s
classifyLine n s         = Many n : classify s

-- | Detect directives, environments, control sequences etc.
classify :: String -> [Class]                           
classify []         = []
classify ('\n' : s) = classifyLine "\n" s
-- Directives look like TeX comments.
-- After recognizing a comment or directive, we continue with
-- 'classifyLine' and thus allow a bird-track section directly
-- after a directive. This is more liberal than Haskell is
-- which always requires a blank line before a bird-track section.
classify ('%'  : s) =
  case encode t of
    Nothing  -> Many ('%' : t ++ arg) : classifyLine "" v -- just a comment
    Just cmd -> Directive cmd arg     : classifyLine "" v
  where
    (t, u)   = break isSpace s
    (arg, v) = breakAfter (== '\n') u

-- | Recognize code sections marked by bird tracks. The first
-- argument takes the character to look for at the beginning of
-- the line. The function returns the recognized part and the rest
-- of the input. A blank line at the end of a bird-track-marked
-- code block is removed.
unbird :: Char -> String -> (String, String)
unbird c xs = go xs
  where
    go :: String -> (String, String) 
    go []                       = ([], [])
    go ('\n' : x : xs) | c == x = '\n' <| ' ' <| go xs
    go ('\n'     : xs)          = '\n' <| blank xs
    go (       x : xs)          = x <| go xs

-- | Remove a blank line.
blank :: String -> (String, String)
blank s
  | all isSpace t = (t,  u)
  | otherwise     = ("", s)
  where (t, u) = breakAfter (== '\n') s

-- | Collapse adjacent occurences of |One| into an occurrence of |Many|.
compress :: [Class] -> [Class]
compress = List.foldr (<|) []
  where
    -- The definition of the operator is carefully written such that
    -- 'compress' is incremental. We produce output once we hit a
    -- newline character.
    (<|) :: Class -> [Class] -> [Class]
    One '\n'          <|           ts  = Many "\n"     : ts
    Many s@('\n' : _) <|           ts  = Many s        : ts
    One c             <| (Many s : ts) = Many (c : s)  : ts
    One c             <|           ts  = Many [c]      : ts
    Many s            <| (Many t : ts) = Many (s ++ t) : ts
    t                 <|           ts  = t             : ts

-- | Adds line numbers to tokens.
number :: LineNumber -> [Class] -> [Numbered Class]
number n []       = []
number n (t : ts) = Numbered n t : number (n + inc) ts
  where
    -- inc is the number of lines to be added
    inc = case t of
            One c           -> newlines [c]  -- changed from 'impossible'
            Many s          -> newlines s
            Inline s        -> newlines s
            Command _ s     -> newlines s
            Environment _ s -> newlines s
            Directive _ s   -> newlines s
            Error _         -> 0

-- | Counts and returns the number of newline characters in a string.
newlines :: String -> Int
newlines = length . List.filter (== '\n')