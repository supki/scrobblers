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
import           Control.Wire (Wire)
import           Data.Default (Default(..))
import           Data.Serialize (Serialize(..), getWord8, putWord8)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8', encodeUtf8)


-- | Scrobbler type. \"Transforms\" @a@ into @b@ over @m@
type Scrobbler m a b = Wire ScrobblerError m a b

-- | Specialized 'Scrobbler'
type Scrobbler' a b = Scrobbler IO a b


-- | Scrobbler errors
data ScrobblerError
  = NoCandidate
  | NoScrobbles
  | NoDecoding String
  | NoSend
  | NoReceive
    deriving (Show, Read, Eq, Ord)


-- | Track information
data Track = Track
  { _title  :: !Text  -- ^ title
  , _artist :: !Text  -- ^ artist
  , _album  :: !Text  -- ^ album title (optional)
  , _length :: !Int64 -- ^ duration
  } deriving (Show, Read, Eq, Ord)

makeLensesWith ?? ''Track $ defaultRules & generateSignatures .~ False

instance Default Track where
  def = Track
    { _title  = ""
    , _artist = ""
    , _album  = ""
    , _length = 0
    }

instance Serialize Track where
  put tr = do
    put (tr^.title.to encodeUtf8)
    put (tr^.artist.to encodeUtf8)
    put (tr^.album.to encodeUtf8)
    put (tr^.length)
  get = do
    Right ti <- decodeUtf8' <$> get
    Right ar <- decodeUtf8' <$> get
    Right al <- decodeUtf8' <$> get
    le <- get
    return $ def
      & title .~ ti
      & artist .~ ar
      & album .~ al
      & length .~ le

-- | Lens to track title
title :: Lens' Track Text

-- | Lens to track artist
artist :: Lens' Track Text

-- | Lens to track album
album :: Lens' Track Text

-- | Lens to track length
length :: Lens' Track Int64


-- | Change in 'Player' state
--
--
--
-- We need it because we want to capture pausing/stopping
-- the player as well as chaining tracks
data PlayerStateChange a = Started !a | Stopped
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


-- | Some data accompanied with timestamps
data Timed a = Timed
  { _datum :: !a
  , _start :: !Int64 -- ^ track start relative to scrobbler start time
  , _local :: !Int64 -- ^ scrobble timestamp
  } deriving (Show, Read, Eq, Ord)

makeLensesWith ?? ''Timed $ defaultRules & generateSignatures .~ False

-- | Lens to timed datum
datum :: Lens' (Timed a) a

-- | Lens to start time
start :: Lens' (Timed a) Int64

-- | Lens to scrobble timestamp
local :: Lens' (Timed a) Int64

instance Functor Timed where
  fmap f t = t { _datum = f (_datum t) }

instance Default a => Default (Timed a) where
  def = Timed
    { _datum = def
    , _start = 0
    , _local = 0
    }

instance Serialize a => Serialize (Timed a) where
  put t = do
    put (t^.datum)
    put (t^.start)
    put (t^.local)
  get = Timed <$> get <*> get <*> get


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
