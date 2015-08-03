-- | MonadState without the function dependency @m -> s@.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.States
    ( module Control.Monad.State
    , MonadStates(get, put)
    , modify
    , modify'
    , gets
    ) where

import Control.Monad.Reader
import Control.Monad.State hiding (MonadState(get, put, state), modify, modify', gets)
import qualified Control.Monad.State as MTL (get, put)
import Control.Monad.Writer (WriterT)

-- | Copy of 'Control.Monad.State.MonadState' with functional dependency m -> s removed.
class Monad m => MonadStates s m where
    get :: m s
    get = state (\s -> (s, s))
    put :: s -> m ()
    put s = state (\_ -> ((), s))
    state :: (s -> (a, s)) -> m a
    state f = do
      s <- get
      let ~(a, s') = f s
      put s'
      return a

-- | Copy of 'Control.Monad.State.mondify'
modify :: MonadStates s m => (s -> s) -> m ()
modify f = state (\s -> ((), f s))

-- | Copy of 'Control.Monad.State.modify''
modify' :: MonadStates s m => (s -> s) -> m ()
modify' f = state (\s -> let s' = f s in s' `seq` ((), s'))

-- | Copy of 'Control.Monad.State.gets'
gets :: MonadStates s m => (s -> a) -> m a
gets f = do
    s <- get
    return (f s)

instance Monad m => MonadStates s (StateT s m) where
    get = MTL.get
    put = MTL.put

instance (Monad m, MonadStates s m) => MonadStates s (ReaderT r m) where
    get = lift get
    put s = lift $ put s

instance (Monad m, Monoid w, MonadStates s m) => MonadStates s (WriterT w m) where
    get = lift get
    put = lift . put
