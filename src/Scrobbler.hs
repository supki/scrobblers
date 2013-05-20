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

import           Control.Monad.Trans (MonadIO, liftIO)
import           Control.Wire hiding (loop)
import qualified Network.MPD as Y

import Scrobbler.Algorithm
import Scrobbler.Announce
import Scrobbler.Lastfm
import Scrobbler.Types


-- | Application loop
scrobbler :: Credentials -> IO ()
scrobbler cs = forever $ void (Y.withMPD (loop' loop clockSession)) `catch` \(_ :: SomeException) -> return ()
 where
  loop =
    announce . scrobble cs .
    announce . contest .
    announce . updateNowPlaying cs .
    candidate

  loop' :: Wire Error Y.MPD () Success -> Session Y.MPD -> Y.MPD ()
  loop' w' session' = do
    (_, w, session) <- stepSession w' session' ()
    liftIO (threadDelay 1000000)
    loop' w session
