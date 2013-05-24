{-# LANGUAGE OverloadedStrings #-}

import Control.Category
import Control.Concurrent (forkIO)
import Prelude hiding ((.), id)

import Crypto.Cipher.AES
import Network

import Control.Scrobbler
import Control.Scrobbler.Algorithm.MPD


main :: IO ()
main = do
  let k = initKey "0123456789ABCDEF"
      iv = IV "FEDCBA9876543210"
  doWork k iv


doWork :: Key -> IV -> IO ()
doWork k iv = do
  forkIO sender
  forkIO receiver
  worker
 where
  receiver = scrobbler $
    announce . fmap (Successes :: [Track] -> Successes Track) (decrypt k iv) . receive (PortNumber 7447)
  sender = scrobbler $
    send "localhost" (PortNumber 4774) . encrypt k iv . candidate

  worker :: IO ()
  worker = scrobbler $
    send "localhost" (PortNumber 7447) .
    encrypt k iv . scrobble cs . announce . contest . announce . updateNowPlaying cs . decrypt k iv .
    receive (PortNumber 4774)
   where
    cs :: Credentials
    cs = Credentials
      { apiKey     = "__YOUR_API_KEY__"
      , sessionKey = "__YOUR_SESSION_KEY__"
      , secret     = "__YOUR_SECRET__"
      }
