module Scrobbler.Lastfm
  ( announce
  , scrobble
  ) where

import Control.Monad (liftM)
import Prelude hiding ((.), id)

import           Control.Monad.Trans (MonadIO, liftIO)
import           Control.Wire
import           Data.Time (formatTime, getCurrentTime)
import qualified Network.Lastfm as L
import qualified Network.Lastfm.Track as T
import           System.Locale (defaultTimeLocale)

import Scrobbler.Types


-- | Scrobble track
scrobble :: MonadIO m => Credentials -> Wire Error m Track ()
scrobble Credentials { secret = s, apiKey = ak, sessionKey = sk } = mkFixM $
  \_dt tr@Track { _artist = ar, _title = t, _album = al } -> do
    liftIO (putStrLn "* Scrobble:")
    liftIO (putStrLn (pretty tr))
    ts <- (read . formatTime defaultTimeLocale "%s") `liftM` liftIO getCurrentTime
    r <- liftIO . L.lastfm . L.sign s $
      T.scrobble <*> L.artist ar <*> L.track t <*> L.timestamp ts <* L.album al <*>
      L.apiKey ak <*> L.sessionKey sk <* L.json
    case r of
      Just _  -> return (Right ())
      Nothing -> return (Left FailedScrobble)


-- | Update user lastfm profile page
announce :: MonadIO m => Credentials -> Wire e m Change Change
announce Credentials { secret = s, apiKey = ak, sessionKey = sk } = mkFixM $ \_dt ch -> do
  liftIO (putStrLn "* Announce:")
  case ch of
    Just tr -> go tr >> liftIO (putStrLn (pretty tr)) >> return (Right ch)
    Nothing -> liftIO (putStrLn "  Player is idle.") >> return (Right ch)
 where
  go Track { _artist = ar, _title = t, _album = al, _length = l } = liftIO . L.lastfm . L.sign s $
    T.updateNowPlaying <*> L.artist ar <*> L.track t <* L.album al <* L.duration l <*>
    L.apiKey ak <*> L.sessionKey sk <* L.json
