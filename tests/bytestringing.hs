{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Control.Wire hiding (unless)
import           Data.String (fromString)
import           Prelude hiding ((.), id)
import           Test.Hspec
import           Test.Hspec.Runner
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Control.Scrobbler.Network
import           Control.Scrobbler.Types


instance Arbitrary Track where
  arbitrary = Track
    <$> (fmap fromString arbitrary)
    <*> (fmap fromString arbitrary)
    <*> (fmap fromString arbitrary)
    <*> arbitrary

instance Arbitrary a => Arbitrary (Stamped a) where
  arbitrary = Stamped
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary


main :: IO ()
main = do
  hspecWith options $ do
    describe "serialization" $
      prop "serialization/deserialization is identity morphism" $ serialization_is_id
 where
  options = defaultConfig { configQuickCheckMaxSuccess = Just 500 }

serialization_is_id :: Stamped Track -> Bool
serialization_is_id t =
  let (et, _) = runIdentity (stepWire serialization mempty (Right t))
  in case et of
    Right t' -> t == t'
    _ -> False

-- serialization/desialization wire
serialization :: Monad m => Scrobbler m (Stamped Track) (Stamped Track)
serialization = deserialize . serialize
