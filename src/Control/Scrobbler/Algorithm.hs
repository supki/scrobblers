-- | Common scrobbling algorithm routines
module Control.Scrobbler.Algorithm (contest, time') where

import Control.Monad (liftM)
import Data.Int (Int64)
import Prelude hiding ((.), id, length)

import Control.Lens
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Wire
import Data.Default (def)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Control.Scrobbler.Types


-- | Check if candidate is ready to be scrobbled
--
-- How to scrobble nicely: <http://www.lastfm.ru/api/scrobbling>
contest :: MonadIO m => Scrobbler m (PlayerStateChange Track) (Scrobble (Timed Track))
contest = contest' . (time' &&& id)

contest' :: MonadIO m => Scrobbler m (Int64, PlayerStateChange Track) (Scrobble (Timed Track))
contest' = mkStateM Stopped $ \_dt ((t, ch), tr) -> do
  lt <- round `liftM` liftIO getPOSIXTime
  return (change (Left NoScrobbles) (go t) tr, ch <&> \tr' -> def
    & datum .~ tr'
    & start .~ t
    & local .~ lt)
 where
  go t tr
    -- If candidate length is less than 30 seconds, we do not scrobble it
    | tr^.datum.length < 30 = Left NoScrobbles
    -- Otherwise, if the time passed since is more than half candidate lenght it's
    -- definitely should be scrobbled
    | dt > tr^.datum.length `div` 2 = Right . Scrobble $ tr & local +~ dt
    -- Otherwise, if the passed is more than 4 minutes it's should be scrobbled anyway
    | dt > 4 * 60 = Right . Scrobble $ tr & local +~ dt
    -- Otherwise there is nothing to scrobble
    | otherwise = Left NoScrobbles
   where
    dt = t - tr^.start

change :: b -> (a -> b) -> PlayerStateChange a -> b
change _ f (Started a) = f a
change b _           _ = b


-- | Rounded time. Somehow useful
time' :: Monad m => Wire e m a Int64
time' = round <$> time
