-- | Generic (and not quite) scrobbler helpers
module Control.Scrobbler.Main
  ( scrobbler
  , announcer
  , link, (==>)
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import Control.Monad (forever, void)
import Control.Monad.Trans (MonadIO, liftIO)
import Prelude hiding ((.), id)

import Control.Wire hiding (loop)
import Data.ByteString (ByteString)

import Control.Scrobbler.Announce
import Control.Scrobbler.Network
import Control.Scrobbler.Types


-- | Generic scrobbler function
--
-- Quite possibly you can just have
--
-- @
-- main :: IO ()
-- main = scrobbler $ ...
-- @
--
-- if you don't need advanced features like network interactions
scrobbler :: Scrobbler' () a -> IO ()
scrobbler loop = forever $ void (loop' loop clockSession) `catchAll` \_ -> return ()
 where
  loop' :: MonadIO m => Scrobbler m () a -> Session m -> m ()
  loop' w' session' = do
    (_, w, session) <- stepSession w' session' ()
    liftIO (threadDelay 1000000)
    loop' w session


-- | Announces successful scrobbles in stdout
announcer :: Announce t => Scrobbler' () t -> IO ()
announcer w = scrobbler (announce . w)
{-# INLINE announcer #-}


-- | Passes data from source to destination
link :: NetworkSettings -- ^ Source
     -> NetworkSettings -- ^ Destination
     -> Scrobbler' ByteString ByteString -> IO ()
link ns ns' w = scrobbler (send ns' . w . receive ns)
{-# INLINE link #-}

-- | Alias for 'link'
(==>) :: NetworkSettings -> NetworkSettings -> Scrobbler' ByteString ByteString -> IO ()
(==>) = link
{-# INLINE (==>) #-}


catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll = catch
{-# INLINE catchAll #-}
