-- | Generic (and not quite) scrobbler helpers
module Control.Scrobbler.Main
  ( scrobbler
  , announcer
  , link, (==>)
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.Catch (MonadCatch, catchAll)
import Control.Monad (forever)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Wire hiding (loop)
import Data.ByteString (ByteString)
import Prelude hiding ((.), id)

import Control.Scrobbler.Announce
import Control.Scrobbler.Network
import Control.Scrobbler.Types


-- | Generic scrobbler loop
--
-- Loops forever, ignoring all exceptions (if any)
-- happening inside the loop
--
-- see @examples/Casual.hs@ for example use
scrobbler :: (Applicative m, MonadCatch m, MonadIO m) => Scrobbler m () a -> m b
scrobbler loop = forever $ loop' loop clockSession_ `catchAll` \_ -> return ()
 where
  loop' w' s' = do
    (dt, s'') <- stepSession s'
    (_,  w'') <- stepWire w' dt (Right ())
    liftIO (threadDelay 1000000)
    loop' w'' s''

-- | Announces successful scrobbles in stdout
announcer :: (Applicative m, MonadCatch m, MonadIO m) => Announce t => Scrobbler m () t -> m ()
announcer w = scrobbler (announce . w)

-- | Passes data from source to destination
link
  :: (Applicative m, MonadCatch m, MonadIO m)
  => NetworkSettings -- ^ Source
  -> NetworkSettings -- ^ Destination
  -> Scrobbler m ByteString ByteString -> m ()
link ns ns' w = scrobbler (send ns' . w . receive ns)

-- | Alias for 'link'
(==>)
  :: (Applicative m, MonadCatch m, MonadIO m)
  => NetworkSettings -> NetworkSettings -> Scrobbler m ByteString ByteString -> m ()
(==>) = link
