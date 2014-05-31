module Remote
  ( toUpdate, toScrobble
  , updatePort, scrobblePort
  ) where

import Control.Lens
import Control.Scrobbler
import Data.Default.Class (def)
import Network


updatePort, scrobblePort :: PortID

updatePort = PortNumber 4774

scrobblePort = PortNumber 4775


remote, toUpdate, toScrobble :: NetworkSettings

remote     = def
  & host     .~ "example.com"

toUpdate   = remote
  & port     .~ updatePort
  & failures .~ Drop

toScrobble = remote
  & port     .~ scrobblePort
