-- | Scrobbler networking. Connects different scrobblers in chain
--
-- Supports optional encryption
module Scrobbler.Network
  ( -- * Networking
    send, receive
    -- * Ennetworking
  , encrypt, encrypt', serialize
    -- * Denetworking
  , decrypt, decrypt', deserialize
  ) where

import Prelude hiding ((.), id)

import qualified Codec.Crypto.RSA as RSA
import           Codec.Crypto.RSA (PublicKey, PrivateKey)
import           Control.Lens
import           Control.Wire
import           Crypto.Random (CryptoRandomGen)
import           Data.ByteString.Lazy (ByteString)
import           Data.Serialize (Serialize, decodeLazy, encodeLazy)
-- import qualified Data.ByteString.Lazy as B

import Scrobbler.Types


-- | Send 'Track' over the wire
send :: Wire e m ByteString ()
send = undefined


-- | Receive 'Track' over the wire
receive :: Wire e m () ByteString
receive = undefined


-- | Encrypt 'Track' with RSA 'Wire'
encrypt :: (CryptoRandomGen g, Monad m) => g -> PublicKey -> Wire e m Track ByteString
encrypt g k = encrypt' g k . serialize
 where

-- | Encrypt 'ByteString' with RSA 'Wire'
encrypt' :: (CryptoRandomGen g, Monad m) => g -> PublicKey -> Wire e m ByteString ByteString
encrypt' g k = mkState g (\_dt -> rsa k)

-- This function is separated from 'encrypt' mainly
-- because I wanted to ensure it does not have access to
-- initial random gen and therefore can not by mistake reuse it
rsa :: CryptoRandomGen g => PublicKey -> (ByteString, g) -> (Either e ByteString, g)
rsa k (bs, s) = RSA.encrypt s k bs & _1 %~ Right


-- | 'Serialize' datum for network transmission
serialize :: (Serialize a, Monad m) => Wire e m a ByteString
serialize = arr encodeLazy


-- | Decrypt 'Track' with RSA 'Wire'
decrypt :: Monad m => PrivateKey -> Wire Error m ByteString Track
decrypt k = deserialize . decrypt' k

-- | Decrypt 'ByteString' with RSA 'Wire'
decrypt' :: Monad m => PrivateKey -> Wire e m ByteString ByteString
decrypt' k = arr (RSA.decrypt k)


-- | De'Serialize' datum after network transmission
deserialize :: (Serialize b, Monad m) => Wire Error m ByteString b
deserialize = mkFix $ \_dt -> left NoDecoding . decodeLazy
