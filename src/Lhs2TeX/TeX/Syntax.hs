module Lhs2TeX.TeX.Syntax where

import Lhs2TeX.Exception
import Lhs2TeX.Directive

-- | The abstract syntax for an lhs2TeX document.
data Class =
    One         Char                -- ^ ordinary character
  | Many        String              -- ^ ordinary text
  | Inline      String              -- ^ inline code
  | Command     Command     String  -- ^ known TeX command with an argument
  | Environment Environment String  -- ^ known TeX environment
  | Directive   Directive   String  -- ^ lhs2TeX directive
  | Error       Exc                 -- ^ parse error

data Command =
    Hs
  | Eval
  | Perform
  | Vrb Bool
  deriving (Eq, Ord, Show)

data Environment =
    Haskell
  | Code
  | Spec
  | Evaluate
  | Hide
  | Ignore
  | Verbatim Bool
  deriving (Eq, Ord, Show)
