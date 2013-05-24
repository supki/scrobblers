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
import qualified Crypto.Cipher.AES as AES
import           Data.ByteString (ByteString)
import           Data.Serialize (Serialize, decode, encode)
import           Network
import qualified Data.ByteString as B

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
encrypt :: (Serialize b, Monad m) => AES.Key -> AES.IV -> Wire e m b ByteString
encrypt k iv = encrypt' k iv . serialize

-- | Encrypt 'ByteString' with RSA 'Wire'
encrypt' :: Monad m => AES.Key -> AES.IV -> Wire e m ByteString ByteString
encrypt' k iv = arr (AES.encryptCTR k iv)


-- | Decrypt 'Track' with RSA 'Wire'
decrypt :: (Serialize b, Monad m) => AES.Key -> AES.IV -> Wire Error m ByteString b
decrypt k iv = deserialize . decrypt' k iv

-- | Decrypt 'ByteString' with RSA 'Wire'
decrypt' :: Monad m => AES.Key -> AES.IV -> Wire e m ByteString ByteString
decrypt' k iv = arr (AES.decryptCTR k iv)


-- | 'Serialize' datum for network transmission
serialize :: (Serialize a, Monad m) => Wire e m a ByteString
serialize = arr encode


-- | De'Serialize' datum after network transmission
deserialize :: (Serialize b, Monad m) => Wire Error m ByteString b
deserialize = mkFix $ \_dt -> left NoDecoding . decode
