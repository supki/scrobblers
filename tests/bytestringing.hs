{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Control.Monad (unless)
import Data.Word (Word8)
import Prelude hiding ((.), id)
import System.Exit (exitFailure)

import           Control.Monad.Trans (MonadIO)
import           Control.Wire hiding (unless)
import           Crypto.Cipher.AES128
import           Crypto.Classes (buildKeyIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Text as T
import           Test.Hspec
import           Test.Hspec.Runner
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import Control.Scrobbler.Network
import Control.Scrobbler.Types


instance Arbitrary Track where
  arbitrary = Track
    <$> (T.pack <$> arbitrary)
    <*> (T.pack <$> arbitrary)
    <*> (T.pack <$> arbitrary)
    <*> arbitrary

instance Arbitrary a => Arbitrary (Timed a) where
  arbitrary = Timed
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary


main :: IO ()
main = do
  k  <- buildKeyIO
  r <- hspecWith options $ do
    describe "serialization" $
      prop "serialization/deserialization is identity morphism" $ serialization_is_id
    describe "encryption" $
      prop "encryption/decryption is identity morphism" $ encryption'_is_id k
  unless (summaryFailures r == 0) exitFailure
 where
  options = defaultConfig { configQuickCheckArgs = stdArgs { maxSuccess = 500 } }


serialization_is_id :: Timed Track -> Bool
serialization_is_id t =
  let (et, _) = stepWireP serialization 0 t
  in case et of
    Right t' -> t == t'
    _ -> False

-- serialization/desialization wire
serialization :: Monad m => Scrobbler m (Timed Track) (Timed Track)
serialization = deserialize . serialize


encryption'_is_id :: AESKey -> [Word8] -> Property
encryption'_is_id k (B.pack -> bs) = monadicIO $ do
  x <- run $ do
    (et, _) <- stepWire (encryption' k) 0 bs
    return $ case et of
      Right bs' -> bs == bs'
      _ -> False
  assert x

-- encryption'/decryption' wire
encryption' :: MonadIO m => AESKey -> Scrobbler m ByteString ByteString
encryption' k = decrypt' k . encrypt' k
