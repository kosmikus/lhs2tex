module Lhs2TeX.Value where

import Lhs2TeX.Utils

type Toggles = Trie Value

-- | Universal datatype for lhs2TeX run-time values.
data Value =
    Undef
  | Str   String
  | Bool  Bool
  | Int   Int
