-- | Reimplementations for stuff that
-- magically disappeared from netwire5
module Control.Scrobbler.Netwire
  ( mkFix
  , mkFixM
  , mkStateM
  ) where

import Control.Monad (liftM)
import Control.Wire
import Prelude hiding ((.))

mkFix :: Monoid s => (s -> a -> Either e b) -> Wire s e m a b
mkFix f = let w = mkPure (\dt -> (\x -> (x, w)) . f dt) in w

mkFixM :: (Monad m, Monoid s) => (s -> a -> m (Either e b)) -> Wire s e m a b
mkFixM f = let w = mkGen (\dt -> liftM (\x -> (x, w)) . f dt) in w

mkStateM :: (Monad m, Monoid t) => s -> (t -> (a, s) -> m (Either e b, s)) -> Wire t e m a b
mkStateM s0 f = go s0
 where
  go s' = mkGen $ \dt x' -> liftM (\(a, b) -> (a, go b)) (f dt (x', s'))
