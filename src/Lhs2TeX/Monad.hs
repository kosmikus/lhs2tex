-- | The monad most of lhs2TeX runs in.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Lhs2TeX.Monad
  (module Lhs2TeX.Monad, module Control.Monad,
   throwError, catchError, get, put, gets, modify, lift, liftIO)
  where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Trans
import Control.Monad.Identity
import System.IO

import Lhs2TeX.State as S
import Lhs2TeX.Exception

newtype Lhs2TeXRaw m a = Lhs2TeX (ErrorT Exc (StateT S.State m) a)
  deriving (Monad,
            Functor,
            MonadIO,
            MonadState S.State,
            MonadError Exc)

-- | The standard variant of the lhs2TeX monad wraps everything
-- around IO.
type Lhs2TeX = Lhs2TeXRaw IO

-- | Many computations that need exception handling and state in
-- lhs2TeX are actually IO-free (we call this pure for simplicity).
-- We express this fact on the type level by keeping the inner
-- monad polymorphic. This added safety is interesting for library
-- use of lhs2TeX, where we might want to have access to lhs2TeX
-- functionality without uncontrolled output.
type Lhs2TeXPure a = forall m. Monad m => Lhs2TeXRaw m a

-- | Run a computation in the lhs2TeX monad.
runLhs2TeX :: S.State -> Lhs2TeX a -> IO (Either Exc a, S.State)
runLhs2TeX st (Lhs2TeX m) = runStateT (runErrorT m) st

-- | Run a pure computation in the lhs2TeX monad.
runLhs2TeXPure :: S.State -> Lhs2TeXPure a -> (Either Exc a, S.State)
runLhs2TeXPure st qm =
  case qm of
    Lhs2TeX m -> runIdentity (runStateT (runErrorT m) st)

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