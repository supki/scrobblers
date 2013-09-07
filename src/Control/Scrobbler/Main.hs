-- | Generic (and not quite) scrobbler helpers
module Control.Scrobbler.Main
  ( scrobbler
  , announcer
  , link, (==>)
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.Catch
import Control.Monad (forever)
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
scrobbler :: (MonadCatch m, MonadIO m) => Scrobbler m () a -> m ()
scrobbler loop = forever $ loop' loop clockSession `catchAll` \_ -> return ()
 where
  loop' w' session' = do
    (_, w, session) <- stepSession w' session' ()
    liftIO (threadDelay 1000000)
    loop' w session


-- | Announces successful scrobbles in stdout
announcer :: (MonadCatch m, MonadIO m) => Announce t => Scrobbler m () t -> m ()
announcer w = scrobbler (announce . w)
{-# INLINE announcer #-}


-- | Passes data from source to destination
link
  :: (MonadCatch m, MonadIO m)
  => NetworkSettings -- ^ Source
  -> NetworkSettings -- ^ Destination
  -> Scrobbler m ByteString ByteString -> m ()
link ns ns' w = scrobbler (send ns' . w . receive ns)
{-# INLINE link #-}

-- | Alias for 'link'
(==>)
  :: (MonadCatch m, MonadIO m)
  => NetworkSettings -> NetworkSettings -> Scrobbler m ByteString ByteString -> m ()
(==>) = link
{-# INLINE (==>) #-}
