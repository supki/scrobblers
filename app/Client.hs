{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)

import Control.Scrobbler
import Control.Scrobbler.Algorithm.MPD
import Data.Serialize (decode)

import Remote


main :: IO ()
main = scrobbler $
    first (send toScrobble . encrypt key . announce . contest)
  . (announce &&& send toUpdate . encrypt key) . candidate
 where
  Right key = decode "__KEY__"
