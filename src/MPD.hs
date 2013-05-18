{-# LANGUAGE OverloadedStrings #-}
module MPD (change) where

import Prelude hiding ((.), id, length)

import           Control.Lens
import           Control.Wire
import           Data.Default (def)
import           Data.Map (Map)
import           Data.Text (Text)
import qualified Network.MPD as Y

import Types


-- | Look for player state changes over time
change :: Wire Error Y.MPD Time Track
change = mkStateM NotPlaying $ \_dt (t, s) ->
  Y.stState `fmap` Y.status >>= \s' -> case (s, s') of
    (NotPlaying, Y.Stopped) -> return (Left NoTrack, NotPlaying)
    (NotPlaying, Y.Paused)  -> return (Left NoTrack, NotPlaying)
    (NotPlaying, Y.Playing) -> do
      Just song <- Y.currentSong
      return (Right (fetchTrackData song & timestamp .~ round t), Playing song (round t))
    (Playing _ _,    Y.Stopped) -> return (Left NoTrack, NotPlaying)
    (Playing _ _,    Y.Paused)  -> return (Left NoTrack, NotPlaying)
    (Playing song _, Y.Playing) -> do
      Just song' <- Y.currentSong
      if song /= song' then do
        return (Right (fetchTrackData song' & timestamp .~ round t), Playing song' (round t))
      else return (Left NoTrack, s)


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
