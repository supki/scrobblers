{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Prelude hiding ((.), id)
import System.Exit (exitFailure)

import           Control.Monad.Trans (MonadIO)
import           Control.Wire
import           Crypto.Cipher.AES128
import           Crypto.Classes (buildKeyIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Text (Text)
import qualified Data.Text as T
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

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
  qc (prop_encryption'_is_id k) >>= \case
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


prop_encryption'_is_id :: AESKey -> ByteString -> Property
prop_encryption'_is_id k bs = monadicIO $ do
  x <- run $ do
    (et, _) <- stepWire (encryption' k) 0 bs
    return $ case et of
      Right bs' -> bs == bs'
      _ -> False
  assert x

-- encryption'/decryption' wire
encryption' :: MonadIO m => AESKey -> Scrobbler m ByteString ByteString
encryption' k = decrypt' k . encrypt' k
