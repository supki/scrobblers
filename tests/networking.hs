{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Prelude hiding ((.), id)
import System.Exit (exitFailure)

import Control.Wire
import Data.Text (Text, pack)
import Test.QuickCheck

import Scrobbler.Network (serialize, deserialize)
import Scrobbler.Types


instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary Track where
  arbitrary = Track
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary


main :: IO ()
main = do
  r <- quickCheckResult prop_serialization_is_id
  case r of
    Failure {} -> exitFailure
    GaveUp {} -> exitFailure
    NoExpectedFailure {} -> exitFailure
    _ -> return ()


-- serialization/desialization wire
serialization :: Monad m => Wire Error m Track Track
serialization = deserialize . serialize


prop_serialization_is_id :: Track -> Bool
prop_serialization_is_id t =
  let (et, _) = stepWireP serialization 0 t
  in case et of
    Right t' -> t == t'
    _ -> False
