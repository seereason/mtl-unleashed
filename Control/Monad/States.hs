-- | MonadState without the function dependency @m -> s@.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.States
    ( MonadStates(getPoly, putPoly)
    , modifyPoly
    , usePoly
    , overPoly
    ) where

import Control.Lens as Lens
{-
import Control.Monad.Reader (ReaderT)
import Control.Monad.State
import Control.Monad.Writer (WriterT)
-}

import Control.Monad.Trans.Cont
import Control.Monad.Trans.Error
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST, get, put, state)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST, get, put, state)
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT, get, put, state)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT, get, put, state)
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict

import Control.Monad.Trans.Class (lift)
import Control.Monad
import Data.Monoid

class Monad m => MonadStates s m where
    -- getPoly :: m s
    -- putPoly :: s -> m ()

    getPoly :: m s
    getPoly = statePoly (\s -> (s, s))

    -- | Replace the state inside the monad.
    putPoly :: s -> m ()
    putPoly s = statePoly (\_ -> ((), s))

    -- | Embed a simple state action into the monad.
    statePoly :: (s -> (a, s)) -> m a
    statePoly f = do
      s <- getPoly
      let ~(a, s') = f s
      putPoly s'
      return a
    {-# MINIMAL statePoly | getPoly, putPoly #-}

instance Monad m => MonadStates s (Strict.StateT s m) where
    getPoly = Strict.get
    putPoly = Strict.put
    statePoly = Strict.state

instance Monad m => MonadStates s (Lazy.StateT s m) where
    getPoly = Lazy.get
    putPoly = Lazy.put
    statePoly = Lazy.state

instance MonadStates s m => MonadStates s (ReaderT r m) where
    getPoly = lift getPoly
    putPoly = lift . putPoly
    statePoly = lift . statePoly

instance (Monoid w, MonadStates s m) => MonadStates s (Lazy.WriterT w m) where
    getPoly = lift getPoly
    putPoly = lift . putPoly
    statePoly = lift . statePoly

instance (Monoid w, MonadStates s m) => MonadStates s (Strict.WriterT w m) where
    getPoly = lift getPoly
    putPoly = lift . putPoly
    statePoly = lift . statePoly

-- new
instance (Monad m, Monoid w) => MonadStates s (LazyRWS.RWST r w s m) where
    getPoly = LazyRWS.get
    putPoly = LazyRWS.put
    statePoly = LazyRWS.state

instance (Monad m, Monoid w) => MonadStates s (StrictRWS.RWST r w s m) where
    getPoly = StrictRWS.get
    putPoly = StrictRWS.put
    statePoly = StrictRWS.state

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers
--
-- All of these instances need UndecidableInstances,
-- because they do not satisfy the coverage condition.

instance MonadStates s m => MonadStates s (ContT r m) where
    getPoly = lift getPoly
    putPoly = lift . putPoly
    statePoly = lift . statePoly

instance (Error e, MonadStates s m) => MonadStates s (ErrorT e m) where
    getPoly = lift getPoly
    putPoly = lift . putPoly
    statePoly = lift . statePoly

instance MonadStates s m => MonadStates s (ExceptT e m) where
    getPoly = lift getPoly
    putPoly = lift . putPoly
    statePoly = lift . statePoly

instance MonadStates s m => MonadStates s (IdentityT m) where
    getPoly = lift getPoly
    putPoly = lift . putPoly
    statePoly = lift . statePoly

instance MonadStates s m => MonadStates s (ListT m) where
    getPoly = lift getPoly
    putPoly = lift . putPoly
    statePoly = lift . statePoly

instance MonadStates s m => MonadStates s (MaybeT m) where
    getPoly = lift getPoly
    putPoly = lift . putPoly
    statePoly = lift . statePoly

modifyPoly :: (Monad m, MonadStates s m) => (s -> s) -> m ()
modifyPoly f = getPoly >>= putPoly . f

usePoly :: forall s m a. (Monad m, MonadStates s m) => Getting a s a -> m a
usePoly lns = (getPoly :: m s) >>= return . view lns

-- | Modify part of the s
overPoly :: forall s m a. (Monad m, MonadStates s m) => ASetter s s a a -> (a -> a) -> m ()
overPoly lns f = getPoly >>= putPoly . over lns f
