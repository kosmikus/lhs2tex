module Lhs2TeX.Directive.Let where

import Control.Applicative

import Lhs2TeX.Utils
import Lhs2TeX.Monad
import Lhs2TeX.Flags
import Lhs2TeX.Value

-- | Evaluation of boolean expressions
eval :: String -> Lhs2TeXPure Value
eval = error "Lhs2TeX.Directive.Let.eval"

parse = error "Lhs2TeX.Directive.Let.parse"

expression :: Lang -> Parser (Toggles -> Value)
expression = error "Lhs2TeX.Directive.Let.expression"
