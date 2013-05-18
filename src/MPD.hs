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
change :: Wire Error Y.MPD Time Change
change = mkStateM Stopped $ \_dt (t, s) ->
  Y.stState `fmap` Y.status >>= \s' -> case (s, s') of
    (Paused,    Y.Stopped) -> return (Right Nothing, Stopped)
    (Playing _, Y.Stopped) -> return (Right Nothing, Stopped)
    (Stopped,   Y.Paused)  -> return (Right Nothing, Paused)
    (Playing _, Y.Paused)  -> return (Right Nothing, Paused)
    (Playing song, Y.Playing) -> do
      Just song' <- Y.currentSong
      if song /= song' then do
        return (Right (Just (fetchTrackData song' & timestamp .~ round t)), Playing song')
      else return (Left NoChange, s)
    (_, Y.Playing) -> do
      Just song <- Y.currentSong
      return (Right (Just (fetchTrackData song & timestamp .~ round t)), Playing song)
    _ -> return (Left NoChange, s)


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
