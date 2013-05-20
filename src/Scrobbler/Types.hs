{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Various types used in scrobbler
module Scrobbler.Types where

import Data.Int (Int64)
import Data.Monoid ((<>))

import           Control.Lens
import           Data.Default (Default(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Network.Lastfm as L
import qualified Network.MPD as Y


-- | Change in 'Player' state
--
-- We need it because we want to capture pausing/stopping
-- the player as well as chaning tracks
data PlayerStateChange a = Started a | Stopped

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

-- | Successfully completed scrobble round
data Success

-- | What to scrobble
newtype Scrobble a = Scrobble a


-- | Scrobbler errors
data Error
  = NoCandidate
  | NoScrobble
  | FailedScrobble
    deriving (Show, Read, Eq, Ord, Enum, Bounded)


class Pretty a where
  pretty :: a -> String

instance Pretty Track where
  pretty Track { _title, _artist, _album } = T.unpack $
    "  " <> _title <> " by " <> _artist <> " from " <> _album

ppretty :: Pretty a => a -> IO ()
ppretty = putStrLn . pretty


data Credentials = Credentials
  { apiKey :: Text
  , sessionKey :: Text
  , secret :: L.Secret
  }
