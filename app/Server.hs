{-# LANGUAGE OverloadedStrings #-}

import Control.Category
import Control.Concurrent (forkIO)
import Prelude hiding ((.), id)

import Control.Scrobbler
import Data.Serialize (decode)

import Remote


main :: IO ()
main = do
  forkIO . announcer $
    updateNowPlaying cs . decrypt key . receive toUpdate
  announcer $
    scrobble cs . decrypt key . receive toScrobble
 where
  cs = Credentials
    { apiKey     = "__API_KEY__"
    , sessionKey = "__SESSION_KEY__"
    , secret     = "__SECRET__"
    }

  Right key = decode "__KEY__"
