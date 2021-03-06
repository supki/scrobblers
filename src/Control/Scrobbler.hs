-- | Lastfm scrobblers making library
--
-- Based on @liblastfm@ and @netwire@ (and other twenty packages)
module Control.Scrobbler
  ( module Control.Scrobbler.Algorithm
  , module Control.Scrobbler.Announce
  , module Control.Scrobbler.Lastfm
  , module Control.Scrobbler.Main
  , module Control.Scrobbler.Network
  , module Control.Scrobbler.Types
  ) where

import Control.Scrobbler.Algorithm (contest)
import Control.Scrobbler.Announce (Announce(..), announce)
import Control.Scrobbler.Lastfm
import Control.Scrobbler.Main
import Control.Scrobbler.Network
import Control.Scrobbler.Types
