module Lhs2TeX.Directive.Subst
  (module Lhs2TeX.Directive.Subst, module Lhs2TeX.Utils)
  where

import Lhs2TeX.Document
import Lhs2TeX.Utils

type Substs = Trie Subst
type Subst  = [Doc] -> Doc
-- TODO: Should the above become `[Doc] -> Lhs2TeX Doc' in order to support
-- exception handling?