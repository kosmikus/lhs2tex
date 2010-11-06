module Lhs2TeX.Exception where

import Control.Monad.Error

type Message = String
type Exc = (Message, String)
instance Error (a,b)
impossible s = error ("the impossible happened in function: " ++ s)
