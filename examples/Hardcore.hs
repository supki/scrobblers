{-# LANGUAGE OverloadedStrings #-}

import Control.Category
import Control.Concurrent (forkIO)
import Prelude hiding ((.), id)

import Network

import Control.Scrobbler
import Control.Scrobbler.Algorithm.MPD


main :: IO ()
main = do
  forkIO sender
  forkIO receiver
  worker
 where
  receiver = scrobbler $
    announce . fmap (Successes :: [Track] -> Successes Track) deserialize . receive (PortNumber 7447)
  sender = scrobbler $
    send "localhost" (PortNumber 4774) . serialize . candidate


worker :: IO ()
worker = scrobbler $
  send "localhost" (PortNumber 7447) .
  serialize . scrobble cs . announce . contest . announce . updateNowPlaying cs . deserialize .
  receive (PortNumber 4774)
 where
  cs :: Credentials
  cs = Credentials
    { apiKey     = "__YOUR_API_KEY__"
    , sessionKey = "__YOUR_SESSION_KEY__"
    , secret     = "__YOUR_SECRET__"
    }
