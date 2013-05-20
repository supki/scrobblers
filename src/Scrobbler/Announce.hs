{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Scrobbler.Announce
  ( Announce(..)
  , pprint
  , announce
  ) where

import Data.Monoid ((<>))
import Prelude hiding ((.), id)

import           Control.Monad.Trans (MonadIO, liftIO)
import           Control.Wire
import qualified Data.Text as T

import Scrobbler.Types


-- | Class of things that can be announced, like
-- changed player state or scrobble round success
class Announce a where
  message :: a -> String

instance Announce Track where
  message Track { _title, _artist, _album } = T.unpack $
    "  " <> _title <> " by " <> _artist <> " from " <> _album

instance Announce a => Announce (PlayerStateChange a) where
  message Stopped     = "* Player is idle"
  message (Started p) = "* Started:\n" <> message p

instance Announce a => Announce (Scrobble a) where
  message (Scrobble p) = "* Scrobble:\n" <> message p

instance Announce Success where
  message Success = "* Successfully scrobbled!"


-- | Announce in 'IO'
pprint :: (MonadIO m, Announce a) => a -> m ()
pprint = liftIO . putStrLn . message


-- | Announcement 'Wire'
announce :: (MonadIO m, Announce a) => Wire e m a a
announce = mkFixM $ \_dt p -> pprint p >> return (Right p)
