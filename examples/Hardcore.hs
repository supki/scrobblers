{-# LANGUAGE OverloadedStrings #-}

import Control.Category
import Control.Concurrent (forkIO)
import Prelude hiding ((.), id)

import qualified Codec.Crypto.RSA as RSA
import           Codec.Crypto.RSA (PublicKey, PrivateKey)
import           Crypto.Random (CryptoRandomGen, SystemRandom, newGenIO)
import           Data.Profunctor (lmap, rmap)
import           Network

import Control.Scrobbler
import Control.Scrobbler.Algorithm.MPD


main :: IO ()
main = do
  g <- newGenIO
  let (k, k', g') = RSA.generateKeyPair g 1024
  doWork k k' (g' :: SystemRandom)


doWork :: CryptoRandomGen g => PublicKey -> PrivateKey -> g -> IO ()
doWork k k' g = do
  forkIO worker
  scrobbler $
    send "localhost" (PortNumber 4774) . lmap unScrobble (encrypt g k) . announce . contest .
    announce . updateNowPlaying credentials . candidate
 where
  worker = scrobbler $
    announce . scrobble credentials . rmap Scrobble (decrypt k') . receive (PortNumber 4774)


credentials :: Credentials
credentials = Credentials
  { apiKey     = "__YOUR_API_KEY__"
  , sessionKey = "__YOUR_SESSION_KEY__"
  , secret     = "__YOUR_SECRET__"
  }

