-- | MonadState without the function dependency @m -> s@.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.States
    ( module Control.Monad.State
    , MonadStates(get, put)
    , modify
    , modify'
    , gets
    , Control.Monad.States.use, Control.Monad.States.iuse, Control.Monad.States.uses, Control.Monad.States.iuses
    ) where

import qualified Control.Lens as Lens (Optical, view, views)
import Control.Lens hiding (view, views, iview, iviews, uses)
import Control.Monad.Readers
import Control.Monad.State hiding (MonadState(get, put, state), modify, modify', gets)
import qualified Control.Monad.State as MTL (get, put)
import Control.Monad.Writer (WriterT)
import Data.Profunctor.Unsafe ((#.), (.#))

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

use :: MonadStates s m => Getting a s a -> m a
use l = Control.Monad.States.gets (Lens.view l)

iuse :: MonadStates s m => IndexedGetting i (i,a) s a -> m (i,a)
iuse l = Control.Monad.States.gets (getConst #. l (Indexed $ \i -> Const #. (,) i))

uses :: (Profunctor p, MonadStates s m) => Lens.Optical p (->) (Const r) s s a a -> p a r -> m r
uses l f = Control.Monad.States.gets (Lens.views l f)

iuses :: MonadStates s m => IndexedGetting i r s a -> (i -> a -> r) -> m r
iuses l = uses l .# Indexed
