-- | The monad most of lhs2TeX runs in.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lhs2TeX.Monad
  (module Lhs2TeX.Monad, module Control.Monad,
   throwError, catchError, get, put, gets, modify, lift, liftIO)
  where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Trans
import System.IO

import Lhs2TeX.State as S
import Lhs2TeX.Exception

newtype Lhs2TeX a = Lhs2TeX (ErrorT Exc (StateT S.State IO) a)
  deriving (Monad,
            Functor,
            MonadIO,
            MonadState S.State,
            MonadError Exc)

-- | Run a computation in the lhs2TeX monad.
runLhs2TeX :: S.State -> Lhs2TeX a -> IO (Either Exc a, S.State)
runLhs2TeX st (Lhs2TeX m) = runStateT (runErrorT m) st

-- | Print an informative message if verbose mode is selected.
info :: String -> Lhs2TeX ()
info txt =
  do
    vrb <- gets verbose
    liftIO $ when vrb $ hPutStrLn stderr txt

-- | Print a string to the output file.
putStrOut :: String -> Lhs2TeX ()
putStrOut txt =
  do
    outfile <- gets output
    liftIO $ hPutStrLn outfile txt

-- | Print a string to the output file and add an extra newline
-- in the end.
putStrOutLn :: String -> Lhs2TeX ()
putStrOutLn txt = putStrOut (txt ++ "\n")