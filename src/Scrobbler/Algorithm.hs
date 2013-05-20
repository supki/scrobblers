-- | Common scrobbling algorithm routines
module Scrobbler.Algorithm (contest, time') where

import Data.Int (Int64)
import Prelude hiding ((.), id, length)

import           Control.Lens
import           Control.Wire

import Scrobbler.Types


-- | Check if candidate is ready to be scrobbled
--
-- How to scrobble nicely: <http://www.lastfm.ru/api/scrobbling>
contest :: Monad m => Wire Error m (PlayerStateChange Track) (Scrobble Track)
contest = contest' . (time' &&& id)

contest' :: Wire Error m (Int64, PlayerStateChange Track) (Scrobble Track)
contest' = mkState Stopped $ \_dt ((t, ch), tr) -> (change (Left NoScrobble) (go t) tr, ch)
 where
  go t tr
    -- If candidate length is less than 30 seconds, we do not scrobble it
    | tr^.length < 30 = Left NoScrobble
    -- Otherwise, if the time passed since is more than half candidate lenght it's
    -- definitely should be scrobbled
    | t - tr^.timestamp > tr^.length `div` 2 = Right (Scrobble tr)
    -- Otherwise, if the passed is more than 4 minutes it's should be scrobbled anyway
    | t - tr^.timestamp > 4 * 60 = Right (Scrobble tr)
    -- Otherwise there is nothing to scrobble
    | otherwise = Left NoScrobble

change :: b -> (a -> b) -> PlayerStateChange a -> b
change _ f (Started a) = f a
change b _           _ = b


-- | Rounded time. Somehow useful
time' :: Monad m => Wire e m a Int64
time' = round <$> time
