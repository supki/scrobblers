{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Various types used in scrobbler
module Types where

import Data.Int (Int64)

import           Control.Lens
import           Data.Default (Default(..))
import           Data.Text (Text)
import qualified Network.MPD as Y


-- | Track information
data Track = Track
  { _timestamp :: Int64  -- ^ playing start timestamp
  , _artist    :: Text   -- ^ artist
  , _title     :: Text   -- ^ title
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

-- | Scrobble state
data Scrobble
  = Started Track -- ^ Candidate for scrobbling record
  | Idle          -- ^ Doing nothing currently
    deriving (Show, Read, Eq, Ord)

-- | Player state
data Player
  = Playing Y.Song Int64 -- ^ Candidate for scrobbling record
  | NotPlaying           -- ^ Being stopped or paused
    deriving (Show, Eq)

-- | Scrobbler errors
data Error
  = NoTrack
  | NoScrobble
    deriving (Show, Read, Eq, Ord, Enum, Bounded)
