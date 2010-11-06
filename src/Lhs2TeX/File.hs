module Lhs2TeX.File where

import System.IO

-- | Open a file for reading, in UTF8 encoding.
readTextFile :: FilePath -> IO String
readTextFile f =
  do
    h <- openFile f ReadMode
    hSetEncoding h utf8
    hGetContents h

-- | Open a file for writing, in UTF8 encoding.
openOutputFile :: FilePath -> IO Handle
openOutputFile f =
  do
    h <- openFile f WriteMode
    hSetEncoding h utf8
    return h