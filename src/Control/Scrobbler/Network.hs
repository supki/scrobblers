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

import qualified Codec.Crypto.RSA as RSA
import           Codec.Crypto.RSA (PublicKey, PrivateKey)
import           Control.Lens
import           Control.Monad.Trans (MonadIO, liftIO)
import           Control.Wire
import           Crypto.Random (CryptoRandomGen)
import           Data.ByteString.Lazy (ByteString)
import           Data.Serialize (Serialize, decodeLazy, encodeLazy)
import           Network
import qualified Data.ByteString.Lazy as B

import Control.Scrobbler.Types


-- | Send serialized 'Track' over the wire
send :: MonadIO m => HostName -> PortID -> Wire Error m ByteString ()
send hn pid = mkFixM $ \_dt bs -> liftIO $ do
  h <- connectTo hn pid
  B.hPut h (B.singleton (fromIntegral (B.length bs)))
  B.hPut h bs
  return (Right ())
 `mplus`
  return (Left NoSend)


-- | Receive 'Track' over the wire
receive :: MonadIO m => PortID -> Wire Error m () ByteString
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


-- | Encrypt 'Track' with RSA 'Wire'
encrypt :: (CryptoRandomGen g, Serialize b, Monad m) => g -> PublicKey -> Wire e m b ByteString
encrypt g k = encrypt' g k . serialize

-- | Encrypt 'ByteString' with RSA 'Wire'
encrypt' :: (CryptoRandomGen g, Monad m) => g -> PublicKey -> Wire e m ByteString ByteString
encrypt' g k = mkState g (\_dt -> rsa k)

-- This function is separated from 'encrypt' mainly
-- because I wanted to ensure it does not have access to
-- initial random gen and therefore can not by mistake reuse it
rsa :: CryptoRandomGen g => PublicKey -> (ByteString, g) -> (Either e ByteString, g)
rsa k (bs, s) = RSA.encrypt s k bs & _1 %~ Right


-- | Decrypt 'Track' with RSA 'Wire'
decrypt :: (Serialize b, Monad m) => PrivateKey -> Wire Error m ByteString b
decrypt k = deserialize . decrypt' k

-- | Decrypt 'ByteString' with RSA 'Wire'
decrypt' :: Monad m => PrivateKey -> Wire e m ByteString ByteString
decrypt' = arr . RSA.decrypt


-- | 'Serialize' datum for network transmission
serialize :: (Serialize a, Monad m) => Wire e m a ByteString
serialize = arr encodeLazy


-- | De'Serialize' datum after network transmission
deserialize :: (Serialize b, Monad m) => Wire Error m ByteString b
deserialize = mkFix $ \_dt -> left NoDecoding . decodeLazy
