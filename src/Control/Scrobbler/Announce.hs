{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Announce things happening in scrobbling process
module Control.Scrobbler.Announce
  ( Announce(..)
  , pprint
  , announce
  ) where

import           Control.Monad.Trans (MonadIO, liftIO)
import           Control.Wire
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.List (intercalate)
import qualified Data.Text as T
import           Prelude hiding ((.), id)

import           Control.Scrobbler.Netwire (mkFixM)
import           Control.Scrobbler.Types


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

instance Announce a => Announce (Stamped a) where
  message (Stamped { _untimed, _local }) = "  at " <> show _local <> "\n" <> message _untimed

instance Announce a => Announce (Scrobble a) where
  message (Scrobble p) = "* Scrobble candidate:\n" <> message p

instance Announce a => Announce (Successes a) where
  message (Successes ps) = intercalate "\n" ("* Successfully scrobbled:" : fmap message ps)

instance Announce ByteString where
  message bs = "* Raw data: " ++ show (B.unpack bs)


-- | 'Announce' in 'IO'
pprint :: (MonadIO m, Announce a) => a -> m ()
pprint = liftIO . putStrLn . message


-- | 'Announce'ment 'Wire'. Propagates input
announce :: (MonadIO m, Monoid t, Announce a) => Wire t e m a a
announce = mkFixM $ \_dt p -> pprint p >> return (Right p)
