{-# LANGUAGE FlexibleContexts #-}
-- | Scrobbling algorithm routines
module Control.Scrobbler.Algorithm
  ( -- * Contesters
    contest, contestWith
    -- * Scrobbling rules
  , Rules
    -- * Auxilliary
  , time'
  ) where

import Control.Lens
import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Wire
import Data.Default.Class (def)
import Data.Int (Int64)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Prelude hiding ((.), id, length)

import Control.Scrobbler.Netwire (mkStateM)
import Control.Scrobbler.Types


-- | Scrobbling rules
--
-- First argument is time elapsed since scrobbler started
--
-- Second argument is contested candidate track
--
-- Result is either some scrobbler failure (most probably 'NoScrobble') or scrobbled track
--
-- Note: users should update '_local' themselves
type Rules = Int64 -> Stamped Track -> Either ScrobblerError (Scrobble (Stamped Track))

-- | Check if candidate is ready to be scrobbled with default 'rules'
contest :: MonadIO m
        => Scrobbler m (PlayerStateChange Track) (Scrobble (Stamped Track))
contest = contestWith rules

-- | Check if candidate is ready to be scrobbled with custom rules
contestWith :: MonadIO m
            => Rules
            -> Scrobbler m (PlayerStateChange Track) (Scrobble (Stamped Track))
contestWith f = contestWith' f . (time' &&& id)

contestWith' :: MonadIO m
             => Rules
             -> Scrobbler m (Int64, PlayerStateChange Track) (Scrobble (Stamped Track))
contestWith' f = mkStateM Stopped $ \_dt ((t, ch), tr) -> do
  lt <- round `liftM` liftIO getPOSIXTime
  return (change (Left NoScrobbles) (f t) tr, ch <&> \tr' -> def
    & untimed .~ tr'
    & start .~ t
    & local .~ lt)

-- | Default rules for scrobbling
--
-- Reference: <http://www.lastfm.ru/api/scrobbling>
rules :: Rules
rules t tr
  -- If candidate length is less than 30 seconds, we do not scrobble it
  | tr^.untimed.length < 30 = Left NoScrobbles
  -- Otherwise, if the time passed since is more than half candidate lenght it's
  -- definitely should be scrobbled
  | dt > tr^.untimed.length `div` 2 = Right . Scrobble $ tr & local +~ dt
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
time' :: Monad m => Wire (Timed NominalDiffTime ()) e m a Int64
time' = round <$> time
