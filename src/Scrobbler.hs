-- | Lastfm scrobbler
--
-- Based on @liblastfm@ and @netwire@ (and other twenty packages)
module Scrobbler
  ( module Scrobbler.Algorithm
  , module Scrobbler.Announce
  , module Scrobbler.Lastfm
  , module Scrobbler.Main
  , module Scrobbler.Types
  ) where

import Scrobbler.Algorithm (contest)
import Scrobbler.Announce (Announce(..), announce)
import Scrobbler.Lastfm (updateNowPlaying, scrobble)
import Scrobbler.Main (scrobbler)
import Scrobbler.Types (Credentials(..))
