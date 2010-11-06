-- | Embedded code evaluation.
module Lhs2TeX.Eval
  (module Lhs2TeX.Eval, module Lhs2TeX.Utils)
  where

import System.IO
import System.Process

import Lhs2TeX.Utils

type Externals   = Trie ProcessInfo
type ProcessInfo = (Handle, Handle, Handle, ProcessHandle)

