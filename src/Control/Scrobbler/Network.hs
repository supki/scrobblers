{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
-- | Scrobbler networking. Connects different scrobblers in chain
--
-- Supports optional encryption
module Control.Scrobbler.Network
  ( -- * Networking
    NetworkSettings(..)
  , Failures(..)
  , defaultNetworkSettings
  , host, port, failures
  , send, receive
  , serialize
  , deserialize
  ) where

import           Control.Lens
import           Control.Monad (liftM, mplus)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Control.Wire
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Sequence (ViewL(..), viewl)
import qualified Data.Sequence as Q
import           Data.Serialize (Serialize, decode, encode)
import           Network
import           Prelude hiding ((.), id)
import           System.Timeout (timeout)

import           Control.Scrobbler.Types
import           Control.Scrobbler.Netwire (mkStateM, mkFix)


-- | Networking settings. Used in 'send' and 'receive'
data NetworkSettings = NetworkSettings
  { _host     :: HostName -- ^ What host send to?
  , _port     :: PortID   -- ^ What port send to/listen on?
  , _failures :: Failures -- ^ What to do with network failures?
  } deriving (Show, Eq)

-- | What to do with network failures?
data Failures = Drop | Preserve
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

makeLensesWith ?? ''NetworkSettings $ lensRules & generateSignatures .~ False

defaultNetworkSettings :: NetworkSettings
defaultNetworkSettings = NetworkSettings
  { _host     = "localhost"
  , _port     = PortNumber 4774
  , _failures = Preserve
  }

-- | Lens to hostname
host :: Lens' NetworkSettings HostName

-- | Lens to port
port :: Lens' NetworkSettings PortID

-- | Lens to network failures policy
failures :: Lens' NetworkSettings Failures

-- | Send serialized 'Track' over the wire
send :: MonadIO m => NetworkSettings -> Scrobbler m ByteString ()
send ns = mkStateM Q.empty $ \_dt (bs, q) -> liftIO $
  case ns^.failures of
    Drop     ->
      (,q) `liftM` single bs
     `mplus`
      return (Left NoSend, q)
    Preserve -> queue (q |> bs)
 where
  single bs = do
    h <- connectTo (ns^.host) (ns^.port)
    B.hPut h (B.singleton (fromIntegral (B.length bs)))
    B.hPut h bs
    return (Right ())

  queue   (viewl ->   EmptyL) = return (Right (), Q.empty)
  queue q@(viewl -> bs :< q') = do
    single bs
    queue q'
   `mplus`
    return (Left NoSend, q)
  queue q = return (Left NoSend, q)

-- | Receive 'Track' over the wire
receive :: MonadIO m => NetworkSettings -> Scrobbler m () ByteString
receive ns = mkStateM Nothing $ \_dt ((), ms) -> liftIO $ do
  s <- maybe (listenOn (ns^.port)) return ms
  r <- timeout 1000000 (accept s)
  case r of
    Just (h, _, _) -> do
      [n] <- B.unpack <$> B.hGet h 1
      bs <- B.hGet h (fromIntegral n)
      return (Right bs, Just s)
    Nothing -> return (Left NoReceive, Just s)
 `mplus`
  return (Left NoReceive, Nothing)

-- | 'Serialize' datum for network transmission
serialize :: (Serialize a, Monad m) => Scrobbler m a ByteString
serialize = arr encode

-- | De'Serialize' datum after network transmission
deserialize :: (Serialize b, Monad m) => Scrobbler m ByteString b
deserialize = mkFix $ \_dt -> left NoDecoding . decode
