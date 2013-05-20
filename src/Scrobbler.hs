{-# LANGUAGE ScopedTypeVariables #-}
module Scrobbler
  ( -- * Application entry point
    scrobbler
    -- * User credentials
  , Credentials(..)
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import Control.Monad (forever, void)
import Prelude hiding ((.), id)

import Control.Wire hiding (loop)

import Scrobbler.Algorithm
import Scrobbler.Algorithm.MPD
import Scrobbler.Announce
import Scrobbler.Lastfm
import Scrobbler.Types


-- | Application loop
scrobbler :: Credentials -> IO ()
scrobbler cs = forever $ void (loop' loop clockSession) `catchAll` \_ -> return ()
 where
  loop =
    announce . scrobble cs .
    announce . contest .
    announce . updateNowPlaying cs .
    candidate


loop' :: Wire Error IO () Success -> Session IO -> IO ()
loop' w' session' = do
  (_, w, session) <- stepSession w' session' ()
  threadDelay 1000000
  loop' w session


catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll = catch
