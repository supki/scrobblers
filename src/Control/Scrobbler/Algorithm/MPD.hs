-- | Scrobbling algorithm for MPD
module Control.Scrobbler.Algorithm.MPD
  ( candidate
  ) where

import           Control.Lens
import           Control.Monad.Trans (MonadIO, liftIO)
import           Control.Wire
import           Data.Default (def)
import           Data.Int (Int64)
import           Data.Map (Map)
import           Data.Text (Text)
import qualified Network.MPD as MPD
import           Prelude hiding ((.), id, length)
import           System.Timeout (timeout)

import           Control.Scrobbler.Algorithm
import           Control.Scrobbler.Netwire (mkStateM)
import           Control.Scrobbler.Types


-- | MPD state
data MPD
  = Playing MPD.Song Int64 -- ^ Candidate for scrobbling record
  | NotPlaying           -- ^ Being stopped or paused
    deriving (Show, Eq)

-- | Look for player state changes over time
candidate :: MonadIO m => Scrobbler m () (PlayerStateChange Track)
candidate = candidate' . time'

candidate' :: MonadIO m => Scrobbler m Int64 (PlayerStateChange Track)
candidate' = mkStateM NotPlaying $ \_dt (t, s) -> liftIO $ do
  -- Wait 5 seconds to complete query. That should be enough
  r <- timeout 5000000 . MPD.withMPD $ do
    MPD.stState `fmap` MPD.status >>= \s' -> case (s, s') of
      -- If we were not playing and are not playing now there is no candidate to send
      (NotPlaying, MPD.Stopped) -> return (Left NoCandidate, NotPlaying)
      (NotPlaying, MPD.Paused)  -> return (Left NoCandidate, NotPlaying)
      -- If we started playing just now there is a candidate to send
      (NotPlaying, MPD.Playing) -> do
        Just song <- MPD.currentSong
        return (Right (Started (fetchTrackData song)), Playing song t)
      -- If we were playing and are not playing now we send that change
      (Playing _ _,     MPD.Stopped) -> return (Right Stopped, NotPlaying)
      (Playing _ _,     MPD.Paused)  -> return (Right Stopped, NotPlaying)
      -- If we were playing and are playing everything become complicated
      (Playing song ts, MPD.Playing) -> do
        Just song' <- MPD.currentSong
        -- If the songs are different, then new song is a candidate to send
        if song /= song' then
          return (Right (Started (fetchTrackData song')), Playing song' t)
        else
          -- Otherwise, if song has been played more then its duration
          -- it means that its looped, so we send it as candidate once again
          let ts' = ts + fromIntegral (MPD.sgLength song)
          in return $ if ts + fromIntegral (MPD.sgLength song) < t
            then (Right (Started (fetchTrackData song')), Playing song' ts')
            -- Otherwise, there is no candidate to send
            else (Left NoCandidate, s)
  case r of
    Just (Right v) -> return v
    _ -> return (Left NoCandidate, NotPlaying)

fetchTrackData :: MPD.Song -> Track
fetchTrackData s =
  let song_tags = MPD.sgTags s
      song_length = fromIntegral (MPD.sgLength s)
  in def
    & artist .~ getTag MPD.Artist song_tags
    & title  .~ getTag MPD.Title song_tags
    & album  .~ getTag MPD.Album song_tags
    & length .~ song_length

getTag :: (Ord k, MPD.ToString a) => k -> Map k [a] -> Text
getTag tag tags = tags ^. ix tag . _head . to MPD.toText
