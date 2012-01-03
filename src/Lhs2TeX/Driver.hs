module Lhs2TeX.Driver where

import System.IO
import System.Process
import Data.List as List

import Lhs2TeX.Flags
import Lhs2TeX.State
import Lhs2TeX.TeX.Syntax
import Lhs2TeX.SearchPath
import Lhs2TeX.Monad
import Lhs2TeX.Utils
import Lhs2TeX.Exception
import Lhs2TeX.Formatting

-- | Main lhs2TeX driver. Reads the input file(s) and generates output as
-- directed by the selected style. Essentially is a simple wrapper around
-- the 'formatStr' function. TODO: bugfix. stopping externals should
-- also happen on abnormal program termination.
lhs2TeX :: Style -> State -> [Class] -> [FilePath] -> IO ()
lhs2TeX style state dirs files =
  do
    expandedPath     <- expandPath (searchpath state)
                                     -- we expand the searchpath once
    (contents, file) <- input files  -- obtain the file to process
    runLhs2TeX (setupState style file expandedPath state) $
      do
        catchError
          (do
             formats (List.map (Numbered 0) dirs) -- process initial directives
             formatStr (addEndEOF contents)       -- process actual input doc
          )
          reportError
        stopExternals    -- clean up external processes
        closeOutputFile  -- clean up output files
      -- TODO: what about exceptions in the cleanup phase?
    return ()

-- | Print an error message.
reportError :: Exc -> Lhs2TeX ()
reportError (msg, context) =
  error "Lhs2TeX.Driver.reportError"

-- | Normalizes the end of the input string.
addEndEOF :: String -> String
addEndEOF = (++ "%EOF\n") . unlines . lines

-- | Tries to close the output file if it is a file.
closeOutputFile :: Lhs2TeX ()
closeOutputFile =
  do
    outfile <- gets output
    liftIO $
      do
        isTerm  <- hIsTerminalDevice outfile
        unless isTerm (hClose outfile)

-- | Tries to stop external processes. TODO: actually kill the processes
-- gracefully would be better to recover from problematic situations.
stopExternals :: Lhs2TeX ()
stopExternals =
  do
    -- obtain relevant parts of the state
    ex  <- gets externals
    -- obtain the process ids of the processes we have started
    let pids = List.map snd $ toList ex
    unless (List.null pids) $
      do
        info "Stopping external processes."
        liftIO $
          mapM_ (\ (pin, _, _, pid) ->
                    do
                      hPutStrLn pin ":q"  -- end hugs/ghci
                      hFlush pin          -- make sure everything's sent
                      waitForProcess pid  -- hope the process will end
                ) pids

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
preprocess state dirs files = "Lhs2TeX.Driver.preprocess"

data PreprocessorStyle = PGMF | PGML
