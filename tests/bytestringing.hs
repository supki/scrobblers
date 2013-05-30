{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Prelude hiding ((.), id)
import System.Exit (exitFailure)

import           Control.Wire
import           Crypto.Cipher.AES128
import           Crypto.Classes (buildKeyIO)
import           Crypto.Modes (getIVIO)
import           Crypto.Types (IV)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Text (Text)
import qualified Data.Text as T
import           Test.QuickCheck

import Control.Scrobbler.Network
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
main = do
  -- Serialization
  qc prop_serialization_is_id >>= \case
    Failure {} -> exitFailure
    GaveUp {} -> exitFailure
    NoExpectedFailure {} -> exitFailure
    _ -> return ()
  -- Encryption
  k  <- buildKeyIO
  iv <- getIVIO
  qc (prop_encryption'_is_id k iv) >>= \case
    Failure {} -> exitFailure
    GaveUp {} -> exitFailure
    NoExpectedFailure {} -> exitFailure
    _ -> return ()
  -- Serialization + encryption
  qc (prop_encryption_is_id k iv) >>= \case
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
serialization :: Monad m => Scrobbler m Track Track
serialization = deserialize . serialize


prop_encryption'_is_id :: AESKey -> IV AESKey -> ByteString -> Bool
prop_encryption'_is_id k iv bs =
  let (et, _) = stepWireP (encryption' k iv) 0 bs
  in case et of
    Right bs' -> bs == bs'
    _ -> False

-- encryption'/decryption' wire
encryption' :: Monad m => AESKey -> IV AESKey -> Scrobbler m ByteString ByteString
encryption' k iv = decrypt' k iv . encrypt' k iv


prop_encryption_is_id :: AESKey -> IV AESKey -> ByteString -> Bool
prop_encryption_is_id k iv bs =
  let (et, _) = stepWireP (encryption' k iv) 0 bs
  in case et of
    Right bs' -> bs == bs'
    _ -> False

-- encryption'/decryption' wire
encryption :: Monad m => AESKey -> IV AESKey -> Scrobbler m Track Track
encryption k iv = decrypt k iv . encrypt k iv
