{-# LANGUAGE OverloadedStrings #-}

import Control.Category
import Prelude hiding ((.), id)

import Control.Scrobbler
import Control.Scrobbler.Algorithm.MPD


main :: IO ()
main = scrobbler $
  announce . scrobble credentials .
  announce . contest .
  announce . updateNowPlaying credentials .
  candidate


credentials :: Credentials
credentials = Credentials
  { apiKey     = "__YOUR_API_KEY__"
  , sessionKey = "__YOUR_SESSION_KEY__"
  , secret     = "__YOUR_SECRET__"
  }
