{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (unless)
import Prelude hiding ((.), id)
import System.Exit (exitFailure)
import System.Timeout (timeout)

import           Control.Lens
import           Control.Wire hiding (unless)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Default (def)
import qualified Data.Text as T
import           Network
import           Test.Hspec
import           Test.Hspec.Runner
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import Control.Scrobbler.Announce
import Control.Scrobbler.Network
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
  b <- newEmptyMVar
  r <- hspecWith options $ do
    describe "announcements" $
      prop "does nothing with its argument" $ announcement_is_id
    describe "communication" $ do
      it "correctly maintains the queue of failures" $ do
        let ds = ["AAAA", "BBBB", "CCCC", "DDDD"]
        (_, w) <- stepWire (send (def & port .~ PortNumber 4567)) 0 "AAAA"
        (_, w) <- stepWire w 0 "BBBB"
        (_, w) <- stepWire w 0 "CCCC"
        forkIO $ do
          (r, _) <- stepWire (receiver (PortNumber 4567)) 0 ()
          case r of
            Right rs -> putMVar b rs
            Left  _  -> return ()
        threadDelay 100000
        (_, _) <- stepWire w 0 "DDDD"
        ds' <- takeMVar b
        ds' `shouldBe` ds
      it "correctly does not maintain the queue of failures" $ do
        let ds = ["DDDD"]
        (_, w) <- stepWire (send (def & port .~ PortNumber 4568 & failures .~ Drop)) 0 "AAAA"
        (_, w) <- stepWire w 0 "BBBB"
        (_, w) <- stepWire w 0 "CCCC"
        forkIO $ do
          (r, _) <- stepWire (receiver (PortNumber 4568)) 0 ()
          case r of
            Right rs -> putMVar b rs
            Left  _  -> return ()
        threadDelay 100000
        (_, _) <- stepWire w 0 "DDDD"
        ds' <- takeMVar b
        ds' `shouldBe` ds
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


receiver :: PortID -> Wire e IO () [ByteString]
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
