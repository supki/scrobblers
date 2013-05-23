{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Prelude hiding ((.), id)
import System.Exit (exitFailure)

import           Codec.Crypto.RSA (PublicKey, PrivateKey, generateKeyPair)
import           Control.Wire
import           Crypto.Random (SystemRandom, CryptoRandomGen, newGenIO)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           Data.Text (Text)
import qualified Data.Text as T
import           Test.QuickCheck

import Scrobbler.Network
import Scrobbler.Types


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
main = do
  -- Serialization
  qc prop_serialization_is_id >>= \case
    Failure {} -> exitFailure
    GaveUp {} -> exitFailure
    NoExpectedFailure {} -> exitFailure
    _ -> return ()
  -- Encryption
  (k, k', g) <- flip generateKeyPair 1024 <$> newGenIO
  qc (prop_encryption'_is_id k k' (g :: SystemRandom)) >>= \case
    Failure {} -> exitFailure
    GaveUp {} -> exitFailure
    NoExpectedFailure {} -> exitFailure
    _ -> return ()
  -- Serialization + encryption
  (k'', k''', g') <- flip generateKeyPair 1024 <$> newGenIO
  qc (prop_encryption_is_id k'' k''' (g' :: SystemRandom)) >>= \case
    Failure {} -> exitFailure
    GaveUp {} -> exitFailure
    NoExpectedFailure {} -> exitFailure
    _ -> return ()
 where
  qc = quickCheckWithResult stdArgs { maxSuccess = 500 }


prop_serialization_is_id :: Track -> Bool
prop_serialization_is_id t =
  let (et, _) = stepWireP serialization 0 t
  in case et of
    Right t' -> t == t'
    _ -> False

-- serialization/desialization wire
serialization :: Monad m => Wire Error m Track Track
serialization = deserialize . serialize


prop_encryption'_is_id :: CryptoRandomGen g => PublicKey -> PrivateKey -> g -> ByteString -> Bool
prop_encryption'_is_id g k k' bs =
  let (et, _) = stepWireP (encryption' g k k') 0 bs
  in case et of
    Right bs' -> bs == bs'
    _ -> False

-- encryption'/decryption' wire
encryption' :: (CryptoRandomGen g, Monad m)
            => PublicKey -> PrivateKey -> g -> Wire Error m ByteString ByteString
encryption' k k' g = decrypt' k' . encrypt' g k


prop_encryption_is_id :: CryptoRandomGen g => PublicKey -> PrivateKey -> g -> ByteString -> Bool
prop_encryption_is_id g k k' bs =
  let (et, _) = stepWireP (encryption' g k k') 0 bs
  in case et of
    Right bs' -> bs == bs'
    _ -> False

-- encryption'/decryption' wire
encryption :: (CryptoRandomGen g, Monad m)
            => PublicKey -> PrivateKey -> g -> Wire Error m Track Track
encryption k k' g = decrypt k' . encrypt g k
