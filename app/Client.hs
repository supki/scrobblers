{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Arrow
import Control.Category
import Control.Scrobbler
import Control.Scrobbler.Algorithm.MPD
import Data.Serialize (decode)
import Prelude hiding ((.), id)

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
