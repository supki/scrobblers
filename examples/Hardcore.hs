{-# LANGUAGE OverloadedStrings #-}

import Control.Category                -- base
import Control.Concurrent (forkIO)     -- base
import Control.Scrobbler               -- scrobblers
import Control.Scrobbler.Algorithm.MPD -- scrobblers
import Control.Lens                    -- lens
import Crypto.Cipher.AES128            -- cipher-aes128
import Data.Default.Class (def)        -- data-default-class
import Network                         -- network
import Prelude hiding ((.), id)        -- base


-- Scrobbler fancier than Casual.hs
-- Demonstrates network interaction and encryption
main :: IO ()
main = buildKeyIO >>= work


work :: AESKey128 -> IO ()
work k = forkIO sender >> forkIO receiver >> worker
 where
  -- Receives succesful scrobbles and announces them in stdout
  receiver = announcer $
    -- Annotations are needed in some pathologically stupid scrobblers
    (decrypt k . receive network' :: Scrobbler IO () (Successes Track))
  -- Gets candidates from player (MPD in that case) and sends
  -- them encrypted through network to 'worker'
  sender = scrobbler $
    send network . encrypt k . candidate

  -- Worker connects sender and receiver
  worker = network ==> network' $
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

  -- Network settings for send/receive threads
  network, network' :: NetworkSettings
  network  = def & port .~ PortNumber 4774
  network' = def & port .~ PortNumber 7447
