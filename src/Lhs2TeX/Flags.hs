-- | Configuration options governing lhs2TeX's behaviour
module Lhs2TeX.Flags where

import Lhs2TeX.Representation

-- | Per run of lhs2TeX, one style can be selected. The style affects how
-- the document is processed. There are several pseudo-styles that cause
-- lhs2TeX to merely print more or less informative messages.
data Style =
    Version     -- ^ print version
  | Help        -- ^ print help
  | SearchPath  -- ^ print search path
  | Copying     -- ^ print license
  | Warranty    -- ^ print warranty
  | CodeOnly    -- ^ extract code without any processing
  | NewCode     -- ^ extract code, respect formatting
  | Verb        -- ^ TeX formatting, code in verbatim
  | Typewriter  -- ^ TeX formatting, code in typewriter font
  | Poly        -- ^ TeX formatting, advanced alignment of code
  | Math        -- ^ TeX formatting, one alignment column
  | Pre         -- ^ preprocessor mode, similar to NewCode
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | lhs2TeX currently supports two languages: Haskell and Agda. Since
-- lhs2TeX does not really parse the code, this mainly affects the lexing
-- behaviour.
data Lang = Haskell | Agda
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Representation Style where
    representation =
      [ ("tt",         Typewriter),
        ("math",       Math),
        ("poly",       Poly),
        ("verb",       Verb),
        ("code",       CodeOnly),
        ("newcode",    NewCode),
        ("pre",        Pre),
        ("version",    Version),
        ("copying",    Copying),
        ("warranty",   Warranty),
        ("help",       Help),
        ("searchpath", SearchPath) ]

instance Representation Lang where
    representation =
      [ ("haskell", Haskell),
        ("agda",    Agda) ]
