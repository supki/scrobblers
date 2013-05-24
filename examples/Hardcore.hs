{-# LANGUAGE OverloadedStrings #-}

import Control.Category
import Control.Concurrent (forkIO)
import Prelude hiding ((.), id)

import Crypto.Cipher.AES
import Network

import Control.Scrobbler
import Control.Scrobbler.Algorithm.MPD


-- Fancier scrobbler than Casual.hs
-- Demonstrates network interaction and encryption
main :: IO ()
main = do
  -- AES stuff
  let k = initKey "0123456789ABCDEF" -- encryption key
      iv = IV "FEDCBA9876543210" -- initialization vector
  -- Scrobbling
  doWork k iv


doWork :: Key -> IV -> IO ()
doWork k iv = do
  forkIO sender
  forkIO receiver
  worker
 where
  -- Receives succesfull scrobbles and announces them in stdout
  receiver = announcer $
    decrypt k iv . receive (PortNumber 7447)
  -- Gets candidates from player (MPD in that case) and sends
  -- them encrypted through network to worker
  sender = scrobbler $
    send "localhost" (PortNumber 4774) . encrypt k iv . candidate

  -- Worker connects sender and receiver
  worker = PortNumber 4774 ==> ("localhost", PortNumber 7447) $
    -- and also does all hard work (see Casual.hs if anything is unclear here)
    encrypt k iv . scrobble cs . announce . contest . announce . updateNowPlaying cs . decrypt k iv
   where
    -- Lastfm credentials. Easy to get with 'liblastfm'
    cs :: Credentials
    cs = Credentials
      { apiKey     = "__YOUR_API_KEY__"
      , sessionKey = "__YOUR_SESSION_KEY__"
      , secret     = "__YOUR_SECRET__"
      }
