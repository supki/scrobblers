module Scrobbler.Network
  ( send, receive
  , encrypt, serialize
  , decrypt, deserialize
  ) where

import Prelude hiding ((.), id)

import           Control.Wire
import           Data.ByteString.Lazy (ByteString)
-- import qualified Data.ByteString.Lazy as B

import Scrobbler.Types


-- | Send 'Track' over the wire
send :: Wire e m ByteString ()
send = undefined


-- | Receive 'Track' over the wire
receive :: Wire e m () ByteString
receive = undefined


-- | Encrypt 'Track' with RSA
encrypt :: Monad m => PublicKey -> Wire e m Track ByteString
encrypt k = arr (encrypt' k) . serialize
 where
  encrypt' :: PublicKey -> ByteString -> ByteString
  encrypt' = undefined


-- | Serialize 'Track' for network transmission
serialize :: Monad m => Wire e m Track ByteString
serialize = arr undefined


-- | Encrypt 'Track' with RSA
decrypt :: Monad m => PrivateKey -> Wire e m ByteString Track
decrypt k = deserialize . arr (decrypt' k)
 where
  decrypt' :: PrivateKey -> ByteString -> ByteString
  decrypt' = undefined


-- | Deserialize 'Track' after network transmission
deserialize :: Monad m => Wire e m ByteString Track
deserialize = arr undefined
