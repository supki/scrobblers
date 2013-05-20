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
import Data.Int (Int64)
import Prelude hiding ((.), id)

import           Control.Monad.Trans (MonadIO, liftIO)
import           Control.Wire
import qualified Network.MPD as Y

import Scrobbler.Algorithm
import Scrobbler.Lastfm
import Scrobbler.Types


-- | Application loop
scrobbler :: Credentials -> IO ()
scrobbler cs = forever $
  void (Y.withMPD (loop' (scrobble cs . contest . announce cs . candidate . time') clockSession))
 `catch`
  \(_ :: SomeException) -> return ()
 where
  loop' :: Wire Error Y.MPD () () -> Session Y.MPD -> Y.MPD ()
  loop' w' session' = do
    (mx, w, session) <- stepSession w' session' ()
    case mx of
      Right () -> liftIO (putStrLn "* Successfully scrobbled!")
      _        -> return ()
    liftIO (threadDelay 1000000)
    loop' w session


time' :: Monad m => Wire e m () Int64
time' = round <$> time
