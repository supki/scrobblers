{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Various types used in scrobbler
module Scrobbler.Types where

import Data.Int (Int64)

import           Control.Lens
import           Data.Default (Default(..))
import           Data.Text (Text)
import qualified Network.MPD as Y

-- | Change in 'Player' behaviour
--
-- We need it because we want to capture pausing/stopping
-- the player as well as chaning tracks
type Change = Maybe Track

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

-- | Player state
data Player
  = Playing Y.Song Int64 -- ^ Candidate for scrobbling record
  | NotPlaying           -- ^ Being stopped or paused
    deriving (Show, Eq)

-- | Scrobbler errors
data Error
  = NoCandidate
  | NoScrobble
  | FailedScrobble
    deriving (Show, Read, Eq, Ord, Enum, Bounded)
