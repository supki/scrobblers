{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Prelude hiding ((.), id)

import           Control.Wire
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Text (Text)
import qualified Data.Text as T
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import Control.Scrobbler.Announce
import Control.Scrobbler.Types


instance Arbitrary ByteString where
  arbitrary = B.pack <$> arbitrary

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary Track where
  arbitrary = Track
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary


main :: IO ()
main = hspec $
  describe "announcements" $
    prop "does nothing with its argument" $ announcement_is_id

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
