-- | Description of lhs2TeX command line options.
module Lhs2TeX.Options
  (options, compatibilityOptions)
  where

import System.Console.GetOpt

import Lhs2TeX.State     as State
import Lhs2TeX.Flags
import Lhs2TeX.TeX.Syntax (Class(..))
import Lhs2TeX.Directive
import Lhs2TeX.SearchPath
import Lhs2TeX.File

-- | An lhs2TeX command line option can transform the state, it can generate
-- directives that are processed before any file is read, or it can be a style.
type Lhs2TeXOption = OptDescr (State -> IO State,
                               [Class] -> [Class],
                               [Style])

-- | All lhs2TeX command line options.
options :: [Lhs2TeXOption]
options =
  [ help, Lhs2TeX.Options.verbose, version, tt, math, poly, code, newcode, verb,
    haskell, agda, pre, Lhs2TeX.Options.output, Lhs2TeX.Options.fldir, nopragmas,
    Lhs2TeX.Options.align, include, equation, set, unset, modpath,
    Lhs2TeX.Options.path, copying, warranty
  ]

-- | A style generally has no arguments.
mkStyle :: [Char] -> [String] -> Style -> String -> Lhs2TeXOption
mkStyle shorts longs st desc =
  Option shorts longs (NoArg (return, id, [st])) desc

-- | An option that takes no arguments and changes the state.
mkStateFlag :: [Char] -> [String] -> (State -> IO State) -> String -> Lhs2TeXOption
mkStateFlag shorts longs sf desc =
  Option shorts longs (NoArg (sf, id, [])) desc

-- | An option with argument that transforms the state.
mkStateMod :: [Char] -> [String] -> (String -> State -> IO State) ->
              String -> String -> Lhs2TeXOption
mkStateMod shorts longs sf def desc =
  Option shorts longs (ReqArg (\ xs -> (sf xs, id, [])) def) desc

-- | An option that generates a directive.
mkDirective :: [Char] -> [String] -> (String -> Class) -> String -> String ->
               Lhs2TeXOption
mkDirective shorts longs df def desc =
  Option shorts longs (ReqArg (\ xs -> (return, (df xs :), [])) def) desc

help     = mkStyle ['h', '?'] ["help"]       Help
                   "print this help message"
version  = mkStyle ['V'     ] ["version"]    Version
                   "be verbose"
tt       = mkStyle []         ["tt"]         Typewriter
                   "typewriter style"
math     = mkStyle []         ["math"]       Math
                   "(classic) math style"
poly     = mkStyle []         ["poly"]       Poly
                   "poly style (default)"
code     = mkStyle []         ["code"]       CodeOnly
                   "(classic) code style"
newcode  = mkStyle []         ["newcode"]    NewCode
                   "new code style"
verb     = mkStyle []         ["verb"]       Verb
                   "verbatim"
pre      = mkStyle []         ["pre"]        Pre
                   "act as ghc preprocessor"
path     = mkStyle []         ["searchpath"] SearchPath
                   "show searchpath"
copying  = mkStyle []         ["copying"]    Copying
                   "display license"
warranty = mkStyle []         ["warranty"]   Warranty
                   "display warranty info"

verbose   = mkStateFlag ['v'] []
                        (\ s -> return $ s { State.verbose = True })
                        "be verbose"
haskell   = mkStateFlag []    ["haskell"]
                        (\ s -> return $ s { lang = Haskell })
                        "Haskell lexer (default)"
agda      = mkStateFlag []    ["agda"]
                        (\ s -> return $ s { lang = Agda })
                        "Agda lexer"
fldir     = mkStateFlag []    ["file-directives"]
                        (\ s -> return $ s { State.fldir = True })
                        "generate %file directives"
nopragmas = mkStateFlag []    ["no-pragmas"]
                        (\ s -> return $ s { pragmas = False })
                        "no LINE pragmas"

output  = mkStateMod ['o'] ["output"]
                     (\ f s -> openOutputFile f >>= \ h ->
                               return s { State.output = h })
                     "file" "specify output file"
modpath = mkStateMod ['P'] ["path"]
                     (\ p s -> return $
                      s { searchpath = modifySearchPath (searchpath s) p })
                     "path" "modify search path"


align    = mkDirective ['A'] ["align"]   (Directive Align)
                       "col"      "align at <col>"
include  = mkDirective ['i'] ["include"] (Directive Include)
                       "file"     "include <file>"
equation = mkDirective ['l'] ["let"]     (Directive Let)
                       "equation" "assume <equation>"
set      = mkDirective ['s'] ["set"]     (\ s -> Directive Let (s ++ " = True" ))
                       "flag"     "set <flag>"
unset    = mkDirective ['u'] ["unset"]   (\ s -> Directive Let (s ++ " = False"))
                       "flag"     "unset <flag>"

-- | Option parser in ancient times
compatibilityOptions :: [String] -> ([Class], [String])
compatibilityOptions = foldr (<|) ([], [])
  where
    "-align"        <| (ds, s : as) = (Directive Align   s : ds,     as)
    "-i"            <| (ds, s : as) = (Directive Include s : ds,     as)
    "-l"            <| (ds, s : as) = (Directive Let     s : ds,     as)
    ('-' : 'i' : s) <| (ds,     as) = (Directive Include s : ds,     as)
    ('-' : 'l' : s) <| (ds,     as) = (Directive Let     s : ds,     as)
    s               <| (ds,     as) = (                      ds, s : as)