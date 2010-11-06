module Lhs2TeX.Formatting where

import Lhs2TeX.Monad

-- | Format a string.
formatStr :: String -> Lhs2TeX ()
formatStr str = formats (texparse 1 str)

formats = undefined
texparse = undefined