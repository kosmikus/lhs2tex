module Lhs2TeX.Formatting where

import Lhs2TeX.Monad
import Lhs2TeX.TeX.Syntax
import Lhs2TeX.TeX.Parser

-- | Format a string.
formatStr :: String -> Lhs2TeX ()
formatStr str = formats (texparse 1 str)

formats = undefined
texparse = undefined