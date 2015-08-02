-- | MonadState without the function dependency @m -> s@.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.State.Extra
    ( module Control.Monad.State
    , MonadState(get, put)
    , modify
    , gets
    ) where

-- import Control.Monad.RWS (RWST)
import Control.Monad.Reader
import Control.Monad.State hiding (MonadState, get, put, modify, gets)
import qualified Control.Monad.State as MTL (get, put)
-- import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT)

-- | Copy of 'Control.Monad.State.MonadState' with functional dependency m -> s removed.
class Monad m => MonadState s m where
    get :: m s
    put :: s -> m ()

-- | Copy of 'Control.Monad.State.mondify'
modify :: (MonadState a m, MonadState s m) => (a -> s) -> m ()
modify f = get >>= put . f

instance Monad m => MonadState s (StateT s m) where
    get = MTL.get
    put = MTL.put

instance (Monad m, MonadState s m) => MonadState s (ReaderT r m) where
    get = lift get
    put s = lift $ put s

instance (Monad m, Monoid w, MonadState s m) => MonadState s (WriterT w m) where
    get = lift get
    put = lift . put

-- | Copy of 'Control.Monad.State.gets'
gets :: MonadState s m => (s -> a) -> m a
gets f = do
    s <- get
    return (f s)
