{-# LANGUAGE OverloadedStrings #-}

import Control.Category
import Control.Concurrent (forkIO)
import Prelude hiding ((.), id)

import Crypto.Cipher.AES128
import Crypto.Classes (buildKeyIO)
import Network

import Control.Scrobbler
import Control.Scrobbler.Algorithm.MPD


-- Scrobbler fancier than Casual.hs
-- Demonstrates network interaction and encryption
main :: IO ()
main = buildKeyIO >>= work


work :: AESKey -> IO ()
work k = forkIO sender >> forkIO receiver >> worker
 where
  -- Receives succesful scrobbles and announces them in stdout
  receiver = announcer $
    decrypt k . receive (PortNumber 7447)
  -- Gets candidates from player (MPD in that case) and sends
  -- them encrypted through network to 'worker'
  sender = scrobbler $
    send "localhost" (PortNumber 4774) . encrypt k . candidate

  -- Worker connects sender and receiver
  worker = PortNumber 4774 ==> ("localhost", PortNumber 7447) $
    -- and also does all hard work (see examples/Casual.hs if anything is unclear here)
    encrypt k . scrobble cs . announce . contest . announce . updateNowPlaying cs . decrypt k
   where
    -- Lastfm credentials. Easy to get with "liblastfm"
    cs :: Credentials
    cs = Credentials
      { apiKey     = "__YOUR_API_KEY__"
      , sessionKey = "__YOUR_SESSION_KEY__"
      , secret     = "__YOUR_SECRET__"
      }
