module Scrobbler.Network
  ( send, receive
  , encrypt, serialize
  , decrypt, deserialize
  ) where

import Prelude hiding ((.), id)

import qualified Codec.Crypto.RSA as RSA
import           Codec.Crypto.RSA (PublicKey, PrivateKey)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Control.Wire
import           Crypto.Random (SystemRandom, newGenIO)
import           Data.ByteString.Lazy (ByteString)
import           Data.Serialize (decodeLazy, encodeLazy)
-- import qualified Data.ByteString.Lazy as B

import Scrobbler.Types


-- | Send 'Track' over the wire
send :: Wire e m ByteString ()
send = undefined


-- | Receive 'Track' over the wire
receive :: Wire e m () ByteString
receive = undefined


-- | Encrypt 'Track' with RSA
encrypt :: MonadIO m => PublicKey -> Wire e m Track ByteString
encrypt k = encrypt' . serialize
 where
  encrypt' = mkStateM Nothing $ \_dt (bs, s) -> liftIO $ do
    g <- maybe newGenIO return s
    let (bs', g') = RSA.encrypt (g :: SystemRandom) k bs
    return (Right bs', Just g')


-- | 'Serialize' 'Track' for network transmission
serialize :: Monad m => Wire e m Track ByteString
serialize = arr encodeLazy


-- | Encrypt 'Track' with RSA
decrypt :: Monad m => PrivateKey -> Wire Error m ByteString Track
decrypt k = deserialize . arr (RSA.decrypt k)


-- | Deserialize 'Track' after network transmission
deserialize :: Monad m => Wire Error m ByteString Track
deserialize = mkFix $ \_dt -> left NoDecoding . decodeLazy
