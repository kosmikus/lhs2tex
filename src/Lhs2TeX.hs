module Main where

import System.IO
import System.Environment
import System.Console.GetOpt
import Control.Monad

import Lhs2TeX.Help
import Lhs2TeX.Options
import Lhs2TeX.State
import Lhs2TeX.Flags
import Lhs2TeX.Representation
import Lhs2TeX.SearchPath
import Lhs2TeX.Driver
import Lhs2TeX.TeXSyntax

-- | Main program. Only wraps 'mainArgs'.
main :: IO ()
main = getArgs >>= mainArgs

-- | Main args. Takes the command line arguments as an explicit
-- list of strings. Tries to figure out whether the options given
-- make sense superficially and passes the options to an option
-- handler.
mainArgs :: [String] -> IO a
mainArgs args =
  do
    encodingSetup
    case getOpt Permute options args of
      (opts, files, [])    ->
        do
          -- interpret the somewhat broken output of 'getOpt'
          (state, dirs, styles)
            <- foldM (\ (s, d, x) (sf, df, ns) ->
                      do
                        s' <- sf s
                        return (s', df d, ns ++ x))
                     (initialState, [], [])
                     opts
          handleOptions state (reverse dirs) (reverse styles) files
      (_, _, errs)  ->
        do
          hPutStrLn stderr $ concat errs
          hPutStrLn stderr $ "Trying compatibility mode option handling ..."
          compatibilityMode args

-- | In lhs2TeX, we generally assume UTF8 encoding for all files.
encodingSetup :: IO ()
encodingSetup =
  do
    hSetEncoding stdin  utf8
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

-- | Option handler. We try to figure out the style that has been selected.
-- The style governs the way lhs2TeX treats the input file, or what
-- information lhs2TeX should print.
handleOptions :: State -> [Class] -> [Style] -> [FilePath] -> IO a
handleOptions state dirs styles files =
  case styles of
    [Help]       -> quitSuccess usage
    [SearchPath] -> quitSuccess (init . unlines $ searchpath state)
    [Version]    -> quitSuccess programInfo
    [Copying]    -> quitSuccess $ programInfo ++ "\n\n" ++ copying
    [Warranty]   -> quitSuccess $ programInfo ++ "\n\n" ++ warranty
    [Pre]       | length files >= 3
                 -> preprocess state dirs PGMF files -- used as -pgmF -F
    [Pre, Help] | length files >= 3
                 -> preprocess state dirs PGML files -- used as -pgmL (literate)
    [s]          -> lhs2TeX s    state dirs files  -- single normal style selected
    []           -> lhs2TeX Poly state dirs files  -- the default style is 'Poly'
    _            -> quitError $ incompatibleStylesError styles

-- | Only one style is allowed per program run. This function generates
-- a helpful error message for the situation where more than one style
-- has been selected by the user.
incompatibleStylesError :: [Style] -> String
incompatibleStylesError styles =
  "Only one style allowed from: " ++
  unwords (map (\ s -> "--" ++ decode s) styles) ++ "\n"

-- | Compatibility mode: in ancient times, lhs2TeX was interpreting command
-- line options in a different way. If options don't make sense in the new
-- way, we still try the old one.
compatibilityMode :: [String] -> IO a
compatibilityMode args@(('-' : a) : x) =
  case encode a of
    Just sty -> cStyle sty x
    Nothing  -> cStyle Typewriter args
       -- in ancient times, 'Typewriter' was default
  where
    cStyle :: Style -> [String] -> IO a
    cStyle style args =
      let (dirs, files) = compatibilityOptions args
      in  lhs2TeX style initialState dirs files
