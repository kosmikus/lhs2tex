-- | Lhs2TeX directives
module Lhs2TeX.Directive where

import Lhs2TeX.Representation

-- | Directives are commands that instruct lhs2TeX to do something special.
-- They look like TeX comments.
data Directive =
    Format      -- ^ change the formatting of a code token
  | Include     -- ^ include another source file
  | Let         -- ^ set a variable
  | File        -- ^ set filename and line number explicitly, similar to
                -- a LINE pragma
  | Options     -- ^ set the options for interpreter invocations
  | Align       -- ^ math mode only: set the alignment column
  | Separation  -- ^ poly mode only: tweak alignment behaviour
  | Latency     -- ^ poly mode only: tweak alignment behaviour
  | Begin       -- ^ start a group
  | End         -- ^ end a group
  | Subst       -- ^ define substitution of a primitive construct
  | If          -- ^ conditional
  | Elif
  | Else
  | Endif
  | EOF         -- ^ stop processing the input
  deriving (Eq, Ord, Show)

instance Representation Directive where
  representation =
    [ ("format",      Format),
      ("include",     Include),
      ("if",          If),
      ("elif",        Elif),
      ("else",        Else),
      ("endif",       Endif),
      ("let",         Let),
      ("file",        File),
      ("options",     Options),
      ("align",       Align),
      ("separation",  Separation),
      ("latency",     Latency),
      ("{",           Begin),
      ("}",           End),
      ("subst",       Subst),
      ("EOF",         EOF) ]

-- | Conditional directives affect the control flow, therefore we
-- must be able to treat them in a special way. The EOF directive
-- is treated as a conditional directive, because we use it as a
-- trigger to check for unbalanced if statements.
conditional :: Directive -> Bool
conditional If    = True
conditional Elif  = True
conditional Else  = True
conditional Endif = True
conditional EOF   = True
conditional _     = False