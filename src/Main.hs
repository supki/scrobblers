{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (catch)
import Control.Monad (void)
import Prelude hiding ((.), (**), id)

import           Control.Monad.Trans (MonadIO, liftIO)
import           Control.Wire
import           Data.Time (formatTime, getCurrentTime)
import qualified Network.Lastfm as L
import qualified Network.Lastfm.Track as T
import qualified Network.MPD as Y
import           System.Locale (defaultTimeLocale)

import MPD
import Types


ak :: L.Request f L.APIKey
ak = L.apiKey "__YOUR_API_KEY__"

sk :: L.Request f L.SessionKey
sk = L.sessionKey "__YOUR_SESSION_KEY__"

secret :: L.Secret
secret = "__YOUR_SECRET__"


-- | Application loop
main :: IO ()
main =
  void (Y.withMPD (loop' (scrobble . time ** announce . change . time) clockSession))
 `catch`
  \(_ :: SomeException) -> main
 where
  loop' :: Wire Error Y.MPD () Track -> Session Y.MPD -> Y.MPD ()
  loop' w' session' = do
    (mx, w, session) <- stepSession w' session' ()
    case mx of
      Right x -> go x >> io (print x)
      _       -> return ()
    loop' w session

  go Track { _artist = ar, _title = t, _album = al } = do
    ts <- read . formatTime defaultTimeLocale "%s" <$> io getCurrentTime
    io . L.lastfm . L.sign secret $
      T.scrobble <*> L.artist ar <*> L.track t <*> L.timestamp ts <* L.album al <*> ak <*> sk <* L.json


-- | Announce player state change to the whole world
announce :: MonadIO m => Wire e m Track Track
announce = mkFixM $ \_dt ch -> case ch of
  tr -> go tr >> io (print tr) >> return (Right ch)
 where
  go Track { _artist = ar, _title = t, _album = al, _length = l } = io . L.lastfm . L.sign secret $
    T.updateNowPlaying <*> L.artist ar <*> L.track t <* L.album al <* L.duration l <*> ak <*> sk <* L.json


-- | Since player state has changed, probably scrobble is needed
scrobble :: MonadIO m => Wire Error m (Time, Track) Track
scrobble = mkState Idle $ \_dt ((t, tr), s) ->
  case s of
    Idle -> (Left NoScrobble, (Started tr))
    Started tr' ->
      if (round t - _timestamp tr') * 2 > _length tr'
        then (Right tr', (Started tr))
        else (Left NoScrobble, (Started tr))


io :: MonadIO m => IO a -> m a
io = liftIO


infixr 9 **
(**) :: Applicative m => m a -> m b -> m (a, b)
(**) = liftA2 (,)
