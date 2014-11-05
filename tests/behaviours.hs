{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Wire hiding (unless)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Default.Class (def)
import           Data.String (fromString)
import           Network
import           Prelude hiding ((.), id)
import           System.Timeout (timeout)
import           Test.Hspec
import           Test.Hspec.Runner
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Control.Scrobbler.Announce
import           Control.Scrobbler.Netwire (mkFixM)
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
  b <- newEmptyMVar
  hspecWith options $ do
    describe "announcements" $
      prop "does nothing with its argument" $ announcement_is_id
    describe "communication" $ do
      it "correctly maintains the queue of failures" $ do
        let ds = ["AAAA", "BBBB", "CCCC", "DDDD"]
        (_, w) <- stepWire (send (def & port .~ PortNumber 4567)) mempty (Right "AAAA")
        (_, w) <- stepWire w mempty (Right "BBBB")
        (_, w) <- stepWire w mempty (Right "CCCC")
        forkIO $ do
          (r, _) <- stepWire (receiver (PortNumber 4567)) mempty (Right ())
          case r of
            Right rs -> putMVar b rs
            Left  _  -> return ()
        threadDelay 100000
        (_, _) <- stepWire w mempty (Right "DDDD")
        ds' <- takeMVar b
        ds' `shouldBe` ds
      it "correctly does not maintain the queue of failures" $ do
        let ds = ["DDDD"]
        (_, w) <- stepWire (send (def & port .~ PortNumber 4568 & failures .~ Drop)) mempty (Right "AAAA")
        (_, w) <- stepWire w mempty (Right "BBBB")
        (_, w) <- stepWire w mempty (Right "CCCC")
        forkIO $ do
          (r, _) <- stepWire (receiver (PortNumber 4568)) mempty (Right ())
          case r of
            Right rs -> putMVar b rs
            Left  _  -> return ()
        threadDelay 100000
        (_, _) <- stepWire w mempty (Right "DDDD")
        ds' <- takeMVar b
        ds' `shouldBe` ds
 where
  options = defaultConfig { configQuickCheckMaxSuccess = Just 500 }

-- Since 'announce' is sufficiently polymorphic
-- it's enough to check property on 'Track' only
announcement_is_id :: Stamped Track -> Property
announcement_is_id t = monadicIO $ do
  x <- run $ do
    (et, _) <- stepWire announce mempty (Right t)
    return $ case et of
      Right t' -> t == t'
      _ -> False
  assert x

receiver :: PortID -> Wire (Timed NominalDiffTime ()) e IO () [ByteString]
receiver pid = mkFixM $ \_dt () -> do
  s <- listenOn pid
  xs <- go s
  return (Right xs)
 where
  go s = do
    r <- timeout 1000000 (accept s)
    case r of
      Just (h, _, _) -> do
        [n] <- B.unpack <$> B.hGet h 1
        bs <- B.hGet h (fromIntegral n)
        (bs :) <$> go s
      Nothing -> return []
