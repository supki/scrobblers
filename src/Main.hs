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
import           Network.Lastfm hiding (Track, tags)
import qualified Network.Lastfm.Track as T
import qualified Network.MPD as Y
import           System.Locale (defaultTimeLocale)

import MPD
import Types


ak :: Request f APIKey
ak = apiKey "__YOUR_API_KEY__"

sk :: Request f SessionKey
sk = sessionKey "__YOUR_SESSION_KEY__"

secret :: Secret
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
      Right x -> go x
      _       -> return Nothing
    loop' w session

  go Track { _artist = ar, _title = t, _album = al } = do
    ts <- read . formatTime defaultTimeLocale "%s" <$> io getCurrentTime
    io . lastfm . sign secret $
      T.scrobble <*> artist ar <*> track t <*> timestamp ts <* album al <*> ak <*> sk <* json


-- | Announce player state change to the whole world
announce :: MonadIO m => Wire e m Change Change
announce = mkFixM $ \_dt ch -> case ch of
  Nothing -> return (Right ch)
  Just tr -> go tr >> return (Right ch)
 where
  go Track { _artist = ar, _title = t, _album = al, _length = l } = io . lastfm . sign secret $
    T.updateNowPlaying <*> artist ar <*> track t <* album al <* duration l <*> ak <*> sk <* json


-- | Since player state has changed, probably scrobble is needed
scrobble :: MonadIO m => Wire Error m (Time, Change) Track
scrobble = mkState Idle $ \_dt ((t, tr), s) ->
  let s' = maybe Idle Started tr in  case s of
    Idle        -> (Left NoScrobble, s')
    Started tr'
      | (round t - _timestamp tr') * 2 > _length tr' -> (Right tr', s')
      | otherwise -> (Left NoScrobble, s')


io :: MonadIO m => IO a -> m a
io = liftIO


infixr 9 **
(**) :: Applicative m => m a -> m b -> m (a, b)
(**) = liftA2 (,)
