{-# LANGUAGE ScopedTypeVariables #-}
module Scrobbler
  ( -- * Application entry point
    scrobbler
    -- * User credentials
  , Credentials(..)
  , module Scrobbler.Algorithm
  , module Scrobbler.Announce
  , module Scrobbler.Lastfm
  , module Scrobbler.Types
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import Control.Monad (forever, void)
import Prelude hiding ((.), id)

import Control.Wire hiding (loop)

import Scrobbler.Algorithm
import Scrobbler.Announce
import Scrobbler.Lastfm
import Scrobbler.Types


-- | Application loop
scrobbler :: Wire Error IO () Success -> IO ()
scrobbler loop = forever $ void (loop' loop clockSession) `catchAll` \_ -> return ()


loop' :: Wire Error IO () Success -> Session IO -> IO ()
loop' w' session' = do
  (_, w, session) <- stepSession w' session' ()
  threadDelay 1000000
  loop' w session


catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll = catch
