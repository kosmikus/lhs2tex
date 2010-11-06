module Lhs2TeX.Representation where

import Data.Map as M
import Data.List as L

-- | The class Representation allows encoding and decoding into textual
-- representations for near-enumeration types.
class Ord a => Representation a where
    representation  ::  [(String, a)]
    encoder         ::  Map String a
    decoder         ::  Map a String

    encoder  =  fromList                             representation
    decoder  =  fromList (L.map (\ (x, y) -> (y, x)) representation)

encode :: (Representation a) => String -> Maybe a
encode s  =  M.lookup s encoder

decode :: (Eq a, Representation a) => a -> String
decode a  =  decoder ! a

