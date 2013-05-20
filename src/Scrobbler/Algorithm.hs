module Scrobbler.Algorithm (candidate, contest) where

import Data.Int (Int64)
import Prelude hiding ((.), id, length)

import           Control.Lens
import           Control.Wire
import           Data.Default (def)
import           Data.Map (Map)
import           Data.Text (Text)
import qualified Network.MPD as Y

import Scrobbler.Types


-- | MPD state
data MPD
  = Playing Y.Song Int64 -- ^ Candidate for scrobbling record
  | NotPlaying           -- ^ Being stopped or paused
    deriving (Show, Eq)


-- | Look for player state changes over time
candidate :: Wire Error Y.MPD () (PlayerStateChange Track)
candidate = candidate' . time'

candidate' :: Wire Error Y.MPD Int64 (PlayerStateChange Track)
candidate' = mkStateM NotPlaying $ \_dt (t, s) ->
  Y.stState `fmap` Y.status >>= \s' -> case (s, s') of
    -- If we were not playing and are not playing now there is no candidate to send
    (NotPlaying, Y.Stopped) -> return (Left NoCandidate, NotPlaying)
    (NotPlaying, Y.Paused)  -> return (Left NoCandidate, NotPlaying)
    -- If we started playing just now there is a candidate to send
    (NotPlaying, Y.Playing) -> do
      Just song <- Y.currentSong
      return (Right (Started (fetchTrackData song & timestamp .~ t)), Playing song t)
    -- If we were playing and are not playing now we send that change
    (Playing _ _,     Y.Stopped) -> return (Right Stopped, NotPlaying)
    (Playing _ _,     Y.Paused)  -> return (Right Stopped, NotPlaying)
    -- If we were playing and are playing everything become complicated
    (Playing song ts, Y.Playing) -> do
      Just song' <- Y.currentSong
      -- If the songs are different, then new song is a candidate to send
      if song /= song' then
        return (Right (Started (fetchTrackData song' & timestamp .~ t)), Playing song' t)
      else
        -- Otherwise, if song has been played more then its duration
        -- it means that its looped, so we send it as candidate once again
        let ts' = ts + fromIntegral (Y.sgLength song)
        in if ts + fromIntegral (Y.sgLength song) < t
          then return (Right (Started (fetchTrackData song' & timestamp .~ ts')), Playing song' ts')
          -- Otherwise, there is no candidate to send
          else return (Left NoCandidate, s)

fetchTrackData :: Y.Song -> Track
fetchTrackData s =
  let song_tags = Y.sgTags s
      song_length = fromIntegral (Y.sgLength s)
  in def
    & artist .~ getTag Y.Artist song_tags
    & title  .~ getTag Y.Title song_tags
    & album  .~ getTag Y.Album song_tags
    & length .~ song_length

getTag :: (Ord k, Y.ToString a) => k -> Map k [a] -> Text
getTag tag tags = tags ^. ix tag . _head . to Y.toText


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


time' :: Monad m => Wire e m a Int64
time' = round <$> time
