{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Various types used in scrobbler
module Control.Scrobbler.Types where

import Control.Applicative (Applicative(..), (<$>))
import Data.Foldable (Foldable(..))
import Data.Int (Int64)
import Data.Monoid (mempty)
import Prelude hiding (length)

import           Control.Lens
import           Data.Default (Default(..))
import           Data.Serialize (Serialize(..), getWord8, putWord8)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8', encodeUtf8)
import qualified Network.Lastfm as L


-- | Change in 'Player' state
--
--
--
-- We need it because we want to capture pausing/stopping
-- the player as well as chaining tracks
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

instance Serialize a => Serialize (PlayerStateChange a) where
  put (Started a) = putWord8 1 >> put a
  put     Stopped = putWord8 0
  get = do
    tag <- getWord8
    case tag of
      0 -> return Stopped
      _ -> Started <$> get


-- | Track information
data Track = Track
  { _start  :: Int64 -- ^ track start relative to scrobbler start time
  , _title  :: Text  -- ^ title
  , _artist :: Text  -- ^ artist
  , _album  :: Text  -- ^ album title (optional)
  , _length :: Int64 -- ^ duration
  , _local  :: Int64 -- ^ scrobble timestamp
  } deriving (Show, Read, Eq, Ord)

makeLensesWith ?? ''Track $ defaultRules & generateSignatures .~ False

instance Default Track where
  def = Track
    { _start  = 0
    , _artist = ""
    , _title  = ""
    , _album  = ""
    , _length = 0
    , _local  = 0
    }

instance Serialize Track where
  put tr = do
    put (tr^.start)
    put (tr^.title.to encodeUtf8)
    put (tr^.artist.to encodeUtf8)
    put (tr^.album.to encodeUtf8)
    put (tr^.length)
    put (tr^.local)
  get = do
    st <- get
    Right ti <- decodeUtf8' <$> get
    Right ar <- decodeUtf8' <$> get
    Right al <- decodeUtf8' <$> get
    le <- get
    lo <- get
    return $ def
      & start .~ st
      & artist .~ ar
      & title .~ ti
      & album .~ al
      & length .~ le
      & local .~ lo

-- | Lens to track start time
start :: Lens' Track Int64

-- | Lens to track title
title :: Lens' Track Text

-- | Lens to track artist
artist :: Lens' Track Text

-- | Lens to track album
album :: Lens' Track Text

-- | Lens to track length
length :: Lens' Track Int64

-- | Lens to track scrobble timestamp
local :: Lens' Track Int64


-- | Successful scrobbles
newtype Successes a = Successes { unSuccesses :: [a] }
    deriving (Show, Read)

instance Functor Successes where
  fmap f (Successes a) = Successes (fmap f a)

instance Foldable Successes where
  foldMap f (Successes a) = foldMap f a

instance Traversable Successes where
  traverse f (Successes a) = Successes <$> traverse f a

instance Serialize a => Serialize (Successes a) where
  put (Successes a) = put a
  get = Successes <$> get


-- | What to scrobble
newtype Scrobble a = Scrobble { unScrobble :: a }
    deriving (Show, Read)

instance Functor Scrobble where
  fmap f (Scrobble a) = Scrobble (f a)

instance Foldable Scrobble where
  foldMap f (Scrobble a) = f a

instance Traversable Scrobble where
  traverse f (Scrobble a) = Scrobble <$> f a

instance Serialize a => Serialize (Scrobble a) where
  put (Scrobble a) = put a
  get = Scrobble <$> get


-- | Scrobbler errors
data Error
  = NoCandidate
  | NoScrobbles
  | NoDecoding String
  | NoSend
  | NoReceive
    deriving (Show, Read, Eq, Ord)


-- | Lastfm API credentials
data Credentials = Credentials
  { apiKey :: Text
  , sessionKey :: Text
  , secret :: L.Secret
  } deriving (Show)
