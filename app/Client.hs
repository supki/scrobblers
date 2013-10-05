{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)

import Control.Scrobbler
import Control.Scrobbler.Algorithm.MPD
import Data.Serialize (decode)

import Remote

infixr 8 </


main :: IO ()
main = scrobbler $
    send toScrobble . encrypt key . announce . contest
  </ (announce &&& send toUpdate . encrypt key) . candidate
 where
  Right key = decode "__KEY__"


-- | "I don't care about 'the second' member of the tuple" operator
(</) :: Arrow arr => arr b c -> arr a (b, d) -> arr a (c, d)
a </ b = first a . b
