{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Scrobbler.Announce where

import Data.Monoid ((<>))

import qualified Data.Text as T

import Scrobbler.Types


class Pretty a where
  pretty :: a -> String

instance Pretty Track where
  pretty Track { _title, _artist, _album } = T.unpack $
    "  " <> _title <> " by " <> _artist <> " from " <> _album

instance Pretty a => Pretty (PlayerStateChange a) where
  pretty Stopped     = "* Player is idle"
  pretty (Started p) = "* Started:\n" <> pretty p

instance Pretty a => Pretty (Scrobble a) where
  pretty (Scrobble p) = "* Scrobble:\n" <> pretty p

instance Pretty Success where
  pretty Success = "  Successfully scrobbled!"


ppretty :: Pretty a => a -> IO ()
ppretty = putStrLn . pretty
