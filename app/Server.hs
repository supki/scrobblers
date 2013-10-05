{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Category
import Control.Concurrent (forkIO)
import Control.Scrobbler
import Data.Serialize (decode)
import Prelude hiding ((.), id)

import Remote


main :: IO ()
main = do
  putStrLn greeting

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

greeting :: String
greeting = unlines
  [ "Scrobbler server v0.1.0.0"
  , ""
  , "Listening to 'update-now-playing' requests on port" ++ show updatePort
  , "Listening to 'scrobble' requests on port" ++ show scrobblePort
  , ""
  , "Happy scrobbling!"
  ]
