module Remote where

import Control.Lens
import Control.Scrobbler
import Data.Default (def)
import Network


remote, toUpdate, toScrobble :: NetworkSettings
remote     = def & host .~ "__HOST__"
toScrobble = remote & port .~ PortNumber 4775
toUpdate   = remote & port .~ PortNumber 4774 & failures .~ Drop
