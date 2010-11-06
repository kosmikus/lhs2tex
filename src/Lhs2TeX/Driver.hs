module Lhs2TeX.Driver where

import Lhs2TeX.Flags
import Lhs2TeX.State
import Lhs2TeX.TeXSyntax
import Lhs2TeX.SearchPath

-- | Main lhs2TeX driver. Reads the input file(s) and generates output as
-- directed by the selected style. Essentially is a simple wrapper around
-- the 'formatStr' function.
lhs2TeX :: Style -> State -> [Class] -> [FilePath] -> IO a
lhs2TeX style state dirs files =
  do
    expandedPath     <- expandPath (searchpath state)
                                     -- we expand the searchpath once
    (contents, file) <- input files  -- obtain the file to process
    -- TODO: continue here. At this point, we have to initialize the monad.
    error "continue"

-- | From a list of filename arguments, return the contents of the first
-- and ignore the others. If no file or a minus is specified, we assume
-- that stdin is the input. We use lazy IO for obtaining the file contents.
-- TODO: perhaps switch to non-lazy IO. TODO: while handle the first
-- file so different from an include directive?
input :: [String] -> IO (String, FilePath)
input []         = getContents >>= \ s -> return (s, "<stdin>")
input ["-"]      = getContents >>= \ s -> return (s, "<stdin>")
input (file : _) = chaseFile [] file  -- search path does not apply

-- | When preprocessing, we perform a little bit of extra work. TODO: explain.
preprocess :: State -> [Class] -> PreprocessorStyle -> [FilePath] -> IO a
preprocess state dirs files = undefined

data PreprocessorStyle = PGMF | PGML