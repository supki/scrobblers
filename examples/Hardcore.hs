{-# LANGUAGE OverloadedStrings #-}

import Control.Category
import Control.Concurrent (forkIO)
import Prelude hiding ((.), id)

import Crypto.Cipher.AES128
import Crypto.Classes (buildKeyIO)
import Crypto.Modes (getIVIO)
import Crypto.Types (IV)
import Network

import Control.Scrobbler
import Control.Scrobbler.Algorithm.MPD


-- Scrobbler fancier than Casual.hs
-- Demonstrates network interaction and encryption
main :: IO ()
main = do
  -- AES stuff
  k  <- buildKeyIO -- encryption key
  iv <- getIVIO -- initialization vector
  -- we need that too because that's two way communication and using
  -- the same IV twice is unsafe
  iv' <- getIVIO
  -- Scrobbling
  doWork k iv iv'


doWork :: AESKey -> IV AESKey -> IV AESKey -> IO ()
doWork k iv iv' = do
  forkIO sender
  forkIO receiver
  worker
 where
  -- Receives succesful scrobbles and announces them in stdout
  receiver = announcer $
    decrypt k iv' . receive (PortNumber 7447)
  -- Gets candidates from player (MPD in that case) and sends
  -- them encrypted through network to 'worker'
  sender = scrobbler $
    send "localhost" (PortNumber 4774) . encrypt k iv . candidate

  -- Worker connects sender and receiver
  worker = PortNumber 4774 ==> ("localhost", PortNumber 7447) $
    -- and also does all hard work (see examples/Casual.hs if anything is unclear here)
    encrypt k iv' . scrobble cs . announce . contest . announce . updateNowPlaying cs . decrypt k iv
   where
    -- Lastfm credentials. Easy to get with "liblastfm"
    cs :: Credentials
    cs = Credentials
      { apiKey     = "__YOUR_API_KEY__"
      , sessionKey = "__YOUR_SESSION_KEY__"
      , secret     = "__YOUR_SECRET__"
      }
