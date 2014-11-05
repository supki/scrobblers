{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
-- | Lastfm interaction
module Control.Scrobbler.Lastfm
  ( Credentials(..)
  , updateNowPlaying
  , scrobble
  ) where

import           Control.Lens
import           Control.Monad (liftM, void)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Control.Wire
import           Data.Aeson.Lens
import           Data.ByteString.Lazy (fromStrict)
import           Data.Foldable (toList)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as N
import           Data.Text (Text)
import           Network.HTTP.Client (HttpException(..))
import           Network.HTTP.Types
import qualified Network.Lastfm as L
import qualified Network.Lastfm.Track as Track
import           Prelude hiding ((.), id, length)

import           Control.Scrobbler.Netwire (mkStateM, mkFixM)
import           Control.Scrobbler.Types


-- | Lastfm API credentials
data Credentials = Credentials
  { apiKey     :: !Text
  , sessionKey :: !Text
  , secret     :: !L.Secret
  } deriving (Show)

{-# ANN scrobble ("HLint: ignore Avoid lambda" :: String) #-}
-- | Scrobble track
scrobble :: MonadIO m => Credentials -> Scrobbler m (Scrobble (Stamped Track)) (Successes Track)
scrobble Credentials { secret, apiKey, sessionKey } =
  mkStateM [] $ \_dt s -> liftIO (L.withConnection (\conn -> go conn s))
 where
  -- Retries to scrobble 49 last failures in addition
  -- to trying to scrobble the new candidate
  go conn (Scrobble ft, xs) = do
    let (ts, ts') = splitAt 49 xs
    (ss, fs) <- go' conn (ft:|ts)
    return (foldr (\_ _ -> Right (Successes ss)) (Left NoScrobbles) ss, fs ++ ts')

  go' :: L.Connection -> NonEmpty (Stamped Track) -> IO ([Track], [Stamped Track])
  go' conn tss = L.sign secret
    (Track.scrobble (N.map timedTrackToItem tss)
      <*> L.apiKey apiKey <*> L.sessionKey sessionKey <* L.json) &
    L.lastfm conn <&> \case
    -- So last.fm request may fail and there is a couple of reasons for it to do so
      -- We can catch some exception for http-conduit
      Left (L.LastfmHttpError (StatusCodeException (Status { statusCode = c }) hs _))
        -- Status code >= 500 means server error, we hold our judgement on
        | c >= 500 -> ([], toList tss)
        -- Otherwise if we have response
        | Just w <- lookup "Response" hs ->
          -- Check if it's still some server error, we hold the judgement on
          if | server (fromStrict w) -> ([], toList tss)
          -- Otherwise it was a client error and we drop the tracks
          -- (something really bad happened like broken API wrapper or whatever)
             | otherwise -> ([], [])
      -- Otherwise we catched some other exception that's some weird failure and we better abort
      Left _ -> ([], toList tss)
      Right v -> case ignoredScrobbles v of
        -- Check if some scrobbles were ignored, if not that's fine
        [] -> (tss^..folded.untimed, [])
        -- Otherwise drop ignored
        is ->
          ( toList tss^..ifolded.indices (`notElem` is).untimed
          , toList tss^..ifolded.indices (`elem` is)
          )

  server    = maybe False (`elem` [11, 16]) . preview (key "error" . _Number)
  ignoredScrobbles resp =
    resp ^.. (key "scrobbles".key "scrobble"._Array.ifolded<.key "ignoredMessage".key "code"._String.filtered (/= "0")).asIndex

  timedTrackToItem :: Stamped Track -> L.Request f L.Scrobble
  timedTrackToItem t = Track.item
    <*> L.artist    (t^.untimed.artist)
    <*> L.track     (t^.untimed.title)
    <*> L.timestamp (t^.local)
    <*  L.album     (t^.untimed.album)

-- | Update lastfm user profile page 'now playing' status
updateNowPlaying :: MonadIO m
                 => Credentials -> Scrobbler m (PlayerStateChange Track) (PlayerStateChange Track)
updateNowPlaying Credentials { secret = s, apiKey = ak, sessionKey = sk } =
  mkFixM $ \_dt -> liftIO . liftM Right . traverse (\t -> go t >> return t)
 where
  -- We do not care if lastfm request fails, so be it:
  -- User.updateNowPlaying is not essential for scrobbling
  go Track { _artist = ar, _title = t, _album = al, _length = l } =
    void . L.lastfm undefined . L.sign s $
      Track.updateNowPlaying <*> L.artist ar <*> L.track t <* L.album al <* L.duration l <*>
      L.apiKey ak <*> L.sessionKey sk <* L.json
