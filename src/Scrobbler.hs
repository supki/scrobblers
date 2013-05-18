{-# LANGUAGE ScopedTypeVariables #-}
module Scrobbler
  ( -- * Application entry point
    scrobbler
    -- * User credentials
  , Credentials(..)
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import Control.Monad (liftM, void)
import Data.Int (Int64)
import Prelude hiding ((.), (**), id, length)

import           Control.Monad.Trans (MonadIO, liftIO)
import           Control.Wire
import           Data.Text (Text)
import           Data.Time (formatTime, getCurrentTime)
import qualified Network.Lastfm as L
import qualified Network.Lastfm.Track as T
import qualified Network.MPD as Y
import           System.Locale (defaultTimeLocale)

import Scrobbler.Algorithm
import Scrobbler.Types


data Credentials = Credentials
  { apiKey :: Text
  , sessionKey :: Text
  , secret :: L.Secret
  }

-- | Application loop
scrobbler :: Credentials -> IO ()
scrobbler cs =
  void (Y.withMPD (loop' (scrobble cs . contest . time' ** announce cs . candidate . time') clockSession))
 `catch`
  \(_ :: SomeException) -> scrobbler cs
 where
  loop' :: Wire Error Y.MPD () () -> Session Y.MPD -> Y.MPD ()
  loop' w' session' = do
    (mx, w, session) <- stepSession w' session' ()
    case mx of
      Right () -> io (putStrLn "Successfully scrobbled!")
      _        -> return ()
    io (threadDelay 1000000)
    loop' w session

-- | Scrobble track
scrobble :: MonadIO m => Credentials -> Wire Error m Track ()
scrobble Credentials { secret = s, apiKey = ak, sessionKey = sk } = mkFixM $
  \_dt Track { _artist = ar, _title = t, _album = al } -> do
    ts <- (read . formatTime defaultTimeLocale "%s") `liftM` io getCurrentTime
    r <- io . L.lastfm . L.sign s $
      T.scrobble <*> L.artist ar <*> L.track t <*> L.timestamp ts <* L.album al <*>
      L.apiKey ak <*> L.sessionKey sk <* L.json
    case r of
      Just _  -> return (Right ())
      Nothing -> return (Left FailedScrobble)

-- | Update user lastfm profile page
announce :: MonadIO m => Credentials -> Wire e m Change Change
announce Credentials{secret = s, apiKey = ak, sessionKey = sk} = mkFixM $ \_dt ch -> case ch of
  Just tr -> go tr >> io (print tr) >> return (Right ch)
  Nothing -> io (putStrLn "Player is idle") >> return (Right ch)
 where
  go Track { _artist = ar, _title = t, _album = al, _length = l } = io . L.lastfm . L.sign s $
    T.updateNowPlaying <*> L.artist ar <*> L.track t <* L.album al <* L.duration l <*>
    L.apiKey ak <*> L.sessionKey sk <* L.json


io :: MonadIO m => IO a -> m a
io = liftIO


infixr 9 **
(**) :: Applicative m => m a -> m b -> m (a, b)
(**) = liftA2 (,)


time' :: Monad m => Wire e m () Int64
time' = round <$> time
