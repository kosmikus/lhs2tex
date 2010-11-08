-- | Pseudo-parser for TeX
module Lhs2TeX.TeX.Parser where

import Data.Char
import Data.List as List
import Control.Monad

import Lhs2TeX.Utils
import Lhs2TeX.Exception
import Lhs2TeX.TeX.Syntax
import Lhs2TeX.Representation

-- | Hardcoded maximum size for arguments of recognized commands.
-- TODO: fix this.
maxChar :: Int
maxChar = 1000

-- | Hardcoded maximum size of recognized environments. TODO: fix this.
maxLine :: Int
maxLine = 80 * 500

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
classify ('%' : s) =
  case encode t of
    Nothing  -> Many ('%' : t ++ arg) : classifyLine "" v -- just a comment
    Just cmd -> Directive cmd arg     : classifyLine "" v
  where
    (t, u)   = break isSpace s
    (arg, v) = breakAfter (== '\n') u

-- Environments and control sequences both start with a backslash.
classify str@('\\' : s) =

  -- Environments.
  -- We scan for \begin and \end control sequences. Our TeX parser
  -- does not deal with nested environments properly.

  case span isIdChar s of
    ("begin", '{' : t) -> -- found the beginning of an environment
      case span isIdChar t of
        (env, '}' : u) -> -- found the name of the environment
          case encode env of
            Nothing  -> cont -- environment unknown, continue
            Just cmd -> -- we know the environment, can we find its end?
              case match maxLine (isPrefixOf end) u of
                Nothing       -> notFound end str : cont -- end not found
                Just (arg, v) ->
                  -- extract the rest of the input and build known env
                  let (w, x) = blank (drop (length end) v)
                  in  Environment cmd (arg ++ w) : classify x
            where
              end = "\\end{" ++ env ++ "}" -- the string to look for

  -- We interpret inline verbatim commands. One practical reason is
  -- that vertical bars are quite typical separation characters for
  -- the \verb command, and we do not want to treat these as the starters
  -- of inline code. Furthermore, interpreting verbatim on the lhs2TeX
  -- level makes it more flexible than it is in TeX.
  --
  -- Inline verbatim commands are not handled together with all the
  -- other commands, because they use special characters to delimit
  -- their argument, rather than braces.

    ("verb*", c : t) -> verbatim True  c t  -- print explicit spaces
    ("verb" , c : t) -> verbatim False c t  -- no explicit spaces

  -- All other TeX control sequences.
  -- All the TeX commands we recognize (except verb, see above) take
  -- a single argument, delimited by curly braces.

    (cmd, '{' : t) ->
      case encode cmd of
        Nothing  -> cont -- unknown command, continue
        Just cmd -> -- known command, look for one argument
          case nested maxChar 0 t of
            Just (a, u) -> Command cmd a : classify u
            Nothing     -> notFound "matching '}'" str : cont
    ([], '%' : t) -> Many "\\%" : classify t -- TODO: why handle this case?
    _             -> cont

  where
    cont = One '\\' : classify s -- fallback, nothing recognized

    -- Find the argument of a verb command.
    verbatim = undefined

-- Inline code
classify ('|' : '|' : s) = One '|' : classify s  -- escaped vertical bar
classify str@('|' : s) =
  case inline maxChar s of
    Just (arg, t) -> Inline arg : classify t  -- found the end
    Nothing       -> notFound "matching `|'" str : One '|' : classify s

-- Short verbatim
classify ('@' : '@' : s) = One '@' : classify s  -- escaped at
classify str@('Q' : s) =
  case shortverb maxChar s of
    Just (arg, t) -> Command (Vrb False) arg : classify t -- spaces never explicit
    Nothing       -> notFound "matching `@'" str : One '@' : classify s

-- Everything else is not interpreted in any special way
classify (c : s) = One c : classify s


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

-- | Tries to recognize an argument enclosed in matching curly braces.
-- Keeps track of nested, possibly backslash-escape, curly braces,
-- and a maximum length.
nested :: Int -> Int -> String -> Maybe (String, String)
nested mx depth s = go mx s
  where
    go 0  s              = Nothing
    go mx []             = Nothing
    go mx ('}' : s)
      | depth == 0       = return ([], s)
      | otherwise        = liftM ('}' <|) (nested (mx - 1) (depth - 1) s)
    go mx ('{' : s)      = liftM ('{' <|) (nested (mx - 1) (depth + 1) s)
    go mx ('\\' : c : s) = liftM (\ x -> '\\' <| c <| x) (go (mx - 2) s)
    go mx (c : s)        = liftM (c <|) (go (mx - 1) s)

-- | Scans for an occurrence of the given character. The second
-- argument gives the maximal number of characters to look ahead.
-- The separator character may be escaped by itself.
separated :: Char -> Int -> String -> Maybe (String, String)
separated c = go
  where
    go 0 xs                   = Nothing
    go n []                   = Nothing
    go n (x : xs) | x == c    = -- one separator character found
      case xs of
        (y : ys)  | y == c   -> liftM (c <|) (go (n - 2) ys)
                                -- escaped separator character found
        _                    -> return ([], xs) -- end of scan
                  | otherwise = liftM (x <|) (go (n - 1) xs)
                                -- continue scan

-- | An inline code block is separated by vertical bars.
inline :: Int -> String -> Maybe (String, String)
inline = separated '|'

-- | An inline verbatim block is separated by at-characters.
shortverb :: Int -> String -> Maybe (String, String)
shortverb = separated '@'

-- | Characters we allow for TeX control sequences and environment
-- names are letters and the star. It's not important that this
-- matches reality, since we only recognize a small set of commands
-- and environments anyway, and they all fit into this scheme.
isIdChar :: Char -> Bool
isIdChar c = isAlpha c || c == '*'

-- | Generate an error token.
notFound :: String -> String -> Class
notFound what s = Error (what ++ " not found", s)