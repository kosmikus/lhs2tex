module Lhs2TeX.File where

import System.IO

-- | Open a file for writing, in UTF8 encoding.
openOutputFile :: FilePath -> IO Handle
openOutputFile f =
  do
    h <- openFile f WriteMode
    hSetEncoding h utf8
    return h