module Lhs2TeX.Directive.Let where

import Control.Applicative

import Lhs2TeX.Utils
import Lhs2TeX.Monad
import Lhs2TeX.Flags

type Toggles = Trie Value

-- | Universal datatype for lhs2TeX run-time values
data Value =
    Undef
  | Str   String
  | Bool  Bool
  | Int   Int

-- | Evaluation of boolean expressions
eval :: String -> Lhs2TeXPure Value
eval = undefined

parse = undefined
expression :: Lang -> Parser (Toggles -> Value)
expression = undefined