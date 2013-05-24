-- | Main application loop is defined here
module Control.Scrobbler.Main
  ( scrobbler
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import Control.Monad (forever, void)
import Control.Monad.Trans (MonadIO, liftIO)
import Prelude hiding ((.), id)

import Control.Wire hiding (loop)

import Control.Scrobbler.Types (Error)


-- | Application loop
-- scrobbler :: (MonadException m, MonadIO m) => Wire Error m () Success -> m ()
scrobbler :: Wire Error IO () a -> IO ()
scrobbler loop = forever $ void (loop' loop clockSession) `catchAll` \_ -> return ()


loop' :: MonadIO m => Wire Error m () a -> Session m -> m ()
loop' w' session' = do
  (_, w, session) <- stepSession w' session' ()
  liftIO (threadDelay 1000000)
  loop' w session


catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll = catch
