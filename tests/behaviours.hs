{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Control.Monad (unless)
import Prelude hiding ((.), id)
import System.Exit (exitFailure)

import           Control.Wire hiding (unless)
import qualified Data.Text as T
import           Test.Hspec
import           Test.Hspec.Runner
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import Control.Scrobbler.Announce
import Control.Scrobbler.Types


instance Arbitrary Track where
  arbitrary = Track
    <$> arbitrary
    <*> (T.pack <$> arbitrary)
    <*> (T.pack <$> arbitrary)
    <*> (T.pack <$> arbitrary)
    <*> arbitrary
    <*> arbitrary


main :: IO ()
main = do
  r <- hspecWith options $
    describe "announcements" $
      prop "does nothing with its argument" $ announcement_is_id
  unless (summaryFailures r == 0) exitFailure
 where
  options = defaultConfig { configQuickCheckArgs = stdArgs { maxSuccess = 500 } }

-- Since 'announce' is sufficiently polymorphic
-- it's enough to check property on 'Track' only
announcement_is_id :: Track -> Property
announcement_is_id t = monadicIO $ do
  x <- run $ do
    (et, _) <- stepWire announce 0 t
    return $ case et of
      Right t' -> t == t'
      _ -> False
  assert x
