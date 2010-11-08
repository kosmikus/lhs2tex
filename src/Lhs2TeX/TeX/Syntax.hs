module Lhs2TeX.TeX.Syntax where

import Lhs2TeX.Exception
import Lhs2TeX.Directive
import Lhs2TeX.Representation

-- | The abstract syntax for an lhs2TeX document.
data Class =
    One         Char                -- ^ ordinary character
  | Many        String              -- ^ ordinary text
  | Inline      String              -- ^ inline code
  | Command     Command     String  -- ^ known TeX command with an argument
  | Environment Environment String  -- ^ known TeX environment
  | Directive   Directive   String  -- ^ lhs2TeX directive
  | Error       Exc                 -- ^ parse error
  deriving (Show)

-- | Known LaTeX control sequences.
data Command =
    Hs
  | Eval
  | Perform
  | Vrb Bool
  deriving (Eq, Ord, Show)

instance Representation Command where
  representation =
    [ ("hs"     , Hs       ),
      ("eval"   , Eval     ),
      ("perform", Perform  ),
      ("verb*"  , Vrb True ),
      ("verb"   , Vrb False) ]

-- | Known LaTeX environments.
data Environment =
    Haskell
  | Code
  | Spec
  | Evaluate
  | Hide
  | Ignore
  | Verbatim Bool
  deriving (Eq, Ord, Show)

instance Representation Environment where
  representation =
    [ ("haskell"  , Haskell       ),
      ("code"     , Code          ),
      ("spec"     , Spec          ),
      ("evaluate" , Evaluate      ),
      ("hide"     , Hide          ),
      ("ignore"   , Ignore        ),
      ("verbatim*", Verbatim True ),
      ("verbatim" , Verbatim False) ]