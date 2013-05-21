{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Various types used in scrobbler
module Scrobbler.Types where

import Control.Applicative (Applicative(..), (<$>))
import Data.Foldable (Foldable(..))
import Data.Int (Int64)
import Data.Monoid (mempty)

import           Control.Lens
import           Data.ByteString.Lazy (ByteString)
import           Data.Default (Default(..))
import           Data.Text (Text)
import qualified Network.Lastfm as L


-- | Change in 'Player' state
--
-- We need it because we want to capture pausing/stopping
-- the player as well as chaning tracks
data PlayerStateChange a = Started a | Stopped
    deriving (Show, Read)

instance Functor PlayerStateChange where
  fmap f (Started a) = Started (f a)
  fmap _     Stopped = Stopped

instance Foldable PlayerStateChange where
  foldMap f (Started a) = f a
  foldMap _     Stopped = mempty

instance Traversable PlayerStateChange where
  traverse f (Started a) = Started <$> f a
  traverse _     Stopped = pure Stopped


-- | Track information
data Track = Track
  { _timestamp :: Int64  -- ^ playing start timestamp
  , _title     :: Text   -- ^ title
  , _artist    :: Text   -- ^ artist
  , _album     :: Text   -- ^ album title (optional)
  , _length    :: Int64  -- ^ duration
  } deriving (Show, Read, Eq, Ord)

makeLenses ''Track

instance Default Track where
  def = Track
    { _timestamp = 0
    , _artist    = ""
    , _title     = ""
    , _album     = ""
    , _length    = 0
    }


-- | Successful scrobbles
newtype Successes a = Successes [a]
    deriving (Show, Read)

instance Functor Successes where
  fmap f (Successes a) = Successes (fmap f a)

instance Foldable Successes where
  foldMap f (Successes a) = foldMap f a

instance Traversable Successes where
  traverse f (Successes a) = Successes <$> traverse f a


-- | What to scrobble
newtype Scrobble a = Scrobble a
    deriving (Show, Read)

instance Functor Scrobble where
  fmap f (Scrobble a) = Scrobble (f a)

instance Foldable Scrobble where
  foldMap f (Scrobble a) = f a

instance Traversable Scrobble where
  traverse f (Scrobble a) = Scrobble <$> f a


-- | Scrobbler errors
data Error
  = NoCandidate
  | NoScrobbles
  | FailedScrobble
    deriving (Show, Read, Eq, Ord, Enum, Bounded)


-- | Lastfm API credentials
data Credentials = Credentials
  { apiKey :: Text
  , sessionKey :: Text
  , secret :: L.Secret
  } deriving (Show)


-- | For encryption
newtype PublicKey = PublicKey ByteString
    deriving (Show, Read)

-- | For decryption
newtype PrivateKey = PrivateKey ByteString
    deriving (Show, Read)
