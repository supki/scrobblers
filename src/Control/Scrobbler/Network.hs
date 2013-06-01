{-# LANGUAGE ViewPatterns #-}
-- | Scrobbler networking. Connects different scrobblers in chain
--
-- Supports optional encryption
module Control.Scrobbler.Network
  ( -- * Networking
    send, receive
    -- * Ennetworking
  , encrypt, encrypt', serialize
    -- * Denetworking
  , decrypt, decrypt', deserialize
  ) where

import Control.Monad (mplus)
import Prelude hiding ((.), id)
import System.Timeout (timeout)

import           Control.Monad.Trans (MonadIO, liftIO)
import           Control.Wire
import           Crypto.Cipher.AES128
import           Crypto.Classes (BlockCipher(ctr, unCtr))
import           Crypto.Types (IV(..))
import           Crypto.Modes (getIVIO)
import           Data.ByteString (ByteString)
import           Data.Sequence (ViewL(..), (|>), viewl)
import qualified Data.Sequence as Q
import           Data.Serialize (Serialize, decode, encode)
import           Network
import qualified Data.ByteString as B

import Control.Scrobbler.Types


-- | Send serialized 'Track' over the wire
send :: MonadIO m => HostName -> PortID -> Scrobbler m ByteString ()
send hn pid = mkStateM Q.empty $ \_dt (bs, q) -> liftIO $ queue (q |> bs)
 where
  queue   (viewl ->   EmptyL) = return (Right (), Q.empty)
  queue q@(viewl -> bs :< q') = do
    h <- connectTo hn pid
    B.hPut h (B.singleton (fromIntegral (B.length bs)))
    B.hPut h bs
    queue q'
   `mplus`
    return (Left NoSend, q)
  queue q = return (Left NoSend, q)


-- | Receive 'Track' over the wire
receive :: MonadIO m => PortID -> Scrobbler m () ByteString
receive pid = mkStateM Nothing $ \_dt ((), ms) -> liftIO $ do
  s <- maybe (listenOn pid) return ms
  r <- timeout 1000000 (accept s)
  case r of
    Just (h, _, _) -> do
      [n] <- B.unpack <$> B.hGet h 1
      bs <- B.hGet h (fromIntegral n)
      return (Right bs, Just s)
    Nothing -> return (Left NoReceive, Just s)
 `mplus`
  return (Left NoReceive, Nothing)


-- | Encrypt 'Track' with AES-CTR 'Wire'
encrypt :: (Serialize b, MonadIO m) => AESKey -> Scrobbler m b ByteString
encrypt k = encrypt' k . serialize

-- | Encrypt 'ByteString' with AES-CTR 'Wire'
encrypt' :: MonadIO m => AESKey -> Scrobbler m ByteString ByteString
encrypt' k = mkStateM Nothing $ \_dt (bs, s) -> do
  IV iv <- maybe (liftIO getIVIO) return s
  let (bs', iv') = ctr k (IV iv) bs
  return (Right (iv `B.append` bs'), Just iv')


-- | Decrypt 'Track' with AES-CTR 'Wire'
decrypt :: (Serialize b, MonadIO m) => AESKey -> Scrobbler m ByteString b
decrypt k = deserialize . decrypt' k

-- | Decrypt 'ByteString' with AES-CTR 'Wire'
decrypt' :: Monad m => AESKey -> Scrobbler m ByteString ByteString
decrypt' k = mkFix $ \_dt bs ->
  let (bs', iv) = B.splitAt 16 bs
      (bs'', _) = unCtr k (IV iv) bs'
  in (Right bs'')


-- | 'Serialize' datum for network transmission
serialize :: (Serialize a, Monad m) => Scrobbler m a ByteString
serialize = arr encode


-- | De'Serialize' datum after network transmission
deserialize :: (Serialize b, Monad m) => Scrobbler m ByteString b
deserialize = mkFix $ \_dt -> left NoDecoding . decode
