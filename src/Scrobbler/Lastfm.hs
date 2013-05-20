{-# LANGUAGE DataKinds #-}
-- | Lastfm interaction
module Scrobbler.Lastfm
  ( updateNowPlaying
  , scrobble
  ) where

import Control.Exception (try)
import Control.Monad (liftM, void)
import Prelude hiding ((.), id)

import           Control.Monad.Trans (MonadIO, liftIO)
import           Control.Wire
import qualified Data.Aeson as A
import           Data.Time (formatTime, getCurrentTime)
import           Network.HTTP.Conduit (HttpException(..))
import qualified Network.Lastfm as L
import qualified Network.Lastfm.Track as T
import           System.Locale (defaultTimeLocale)

import Scrobbler.Types


-- | Scrobble track
scrobble :: MonadIO m => Credentials -> Wire Error m (Scrobble Track) Success
scrobble Credentials { secret = s, apiKey = ak, sessionKey = sk } = mkFixM $
  \_dt (Scrobble Track { _artist = ar, _title = t, _album = al }) -> liftIO $ do
    ts <- (read . formatTime defaultTimeLocale "%s") `liftM` getCurrentTime
    -- We do not care if lastfm request fails, but actually we should
    r <- tryLastfm . L.sign s $
      T.scrobble <*> L.artist ar <*> L.track t <*> L.timestamp ts <* L.album al <*>
      L.apiKey ak <*> L.sessionKey sk <* L.json
    return $ case r of
      Right _ -> Right Success
      Left _  -> Left FailedScrobble


-- | Update lastfm user profile page 'now playing' status
updateNowPlaying :: MonadIO m => Credentials -> Wire e m (PlayerStateChange Track) (PlayerStateChange Track)
updateNowPlaying Credentials { secret = s, apiKey = ak, sessionKey = sk } = mkFixM $ \_dt ch -> liftIO $ do
  -- Change might be either new candidate or stopped player notification
  case ch of
    -- If it is new candidate we tell lastfm about it and also announce in stddout
    Started tr -> do
      -- We do not care if lastfm request fails, so be it:
      -- User.updateNowPlaying is not essential for scrobbling
      go tr
    Stopped ->
      return ()
  return (Right ch)
 where
  go Track { _artist = ar, _title = t, _album = al, _length = l } = void . tryLastfm . L.sign s $
    T.updateNowPlaying <*> L.artist ar <*> L.track t <* L.album al <* L.duration l <*>
    L.apiKey ak <*> L.sessionKey sk <* L.json


tryLastfm :: L.Request L.JSON L.Ready -> IO (Either HttpException A.Value)
tryLastfm r = do
  q <- try (L.lastfm r)
  return $ case q of
    Right (Just v) -> Right v
    Right Nothing  -> Left (HttpParserException "aeson")
    Left e         -> Left e
