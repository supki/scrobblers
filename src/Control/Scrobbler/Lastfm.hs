{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lastfm interaction
module Control.Scrobbler.Lastfm
  ( Credentials(..)
  , updateNowPlaying
  , scrobble
  ) where

import Control.Exception (try)
import Control.Monad (liftM, void)
import Prelude hiding ((.), id, length)

import           Control.Lens
import           Control.Lens.Aeson
import           Control.Monad.Trans (MonadIO, liftIO)
import           Control.Wire
import qualified Data.Aeson as A
import           Data.ByteString.Lazy (fromStrict)
import           Data.Text (Text)
import           Network.HTTP.Conduit (HttpException(..))
import           Network.HTTP.Types
import qualified Network.Lastfm as L
import qualified Network.Lastfm.Track as T

import Control.Scrobbler.Types


-- | Lastfm API credentials
data Credentials = Credentials
  { apiKey :: Text
  , sessionKey :: Text
  , secret :: L.Secret
  } deriving (Show)


-- | Scrobble track
scrobble :: MonadIO m => Credentials -> Scrobbler m (Scrobble Track) (Successes Track)
scrobble Credentials { secret = s, apiKey = ak, sessionKey = sk } = mkStateM [] $ \_dt -> liftIO . go
 where
  go (Scrobble ft, ts) = do
    (ss, fs) <- go' (ft:ts) [] []
    return (foldr (\_ _ -> Right (Successes ss)) (Left NoScrobbles) ss, fs)

  go' :: [Track] -> [Track] -> [Track] -> IO ([Track], [Track])
  go' tss@(t:ts) ss fs = do
    r <- try . L.lastfm . L.sign s $ T.scrobble <*>
      L.artist (t^.artist) <*> L.track (t^.title) <*> L.timestamp (t^.local) <* L.album (t^.album) <*>
      L.apiKey ak <*> L.sessionKey sk <* L.json
    -- So last.fm request may fail and there is a couple of reasons for it to do so
    case r of
      -- We can catch some exception for http-conduit
      Left (StatusCodeException (Status { statusCode = c }) hs _)
        -- Status code >= 500 means server error, we hold on our judgement
        | c >= 500 -> go' ts ss (t:fs)
        -- Otherwise if we have response
        | Just w <- lookup "Response" hs -> case w of
            -- Check if it's some server error, we hold on the judgement then
          _ | server (fromStrict w) -> go' ts ss (t:fs)
            -- Otherwise it was a client error and we drop the track
            | otherwise -> go' ts ss fs
      -- Otherwise we catched some other exception that's some weird failure and we better abort
      Left _ -> return (reverse ss, tss ++ reverse fs)
      -- If we fail to parse JSON that's some kind of connection issue, abort everything
      Right Nothing -> return (reverse ss, tss ++ reverse fs)
      Right (Just v)
        -- If we found 'ignored' field in JSON response, we can safely ignore this track
        | dismissed v -> go' ts ss fs
        -- Otherwise everything went fine
        | otherwise -> go' ts (t:ss) fs
  go' [] ss fs = return (reverse ss, reverse fs)

  server    = maybe False (`elem` [11, 16]) . preview (key "error" . _Number)
  dismissed = maybe True (/= "0") . preview (key "scrobbles" . key "@attr" . key "ignored" . _String)


-- | Update lastfm user profile page 'now playing' status
updateNowPlaying :: MonadIO m
                 => Credentials -> Scrobbler m (PlayerStateChange Track) (PlayerStateChange Track)
updateNowPlaying Credentials { secret = s, apiKey = ak, sessionKey = sk } =
  mkFixM $ \_dt -> liftIO . liftM Right . traverse (\t -> go t >> return t)
 where
  -- We do not care if lastfm request fails, so be it:
  -- User.updateNowPlaying is not essential for scrobbling
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
