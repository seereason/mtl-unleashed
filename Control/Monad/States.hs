-- | MonadState without the function dependency @m -> s@.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.States
    ( module Control.Monad.State
    , MonadStates(get, put)
    , modify'
    , use'
    , over'
    ) where

import Control.Lens as Lens
import Control.Monad.Reader (ReaderT)
import Control.Monad.State hiding (get, put, modify')
import qualified Control.Monad.State as MTL (get, put)
import Control.Monad.Writer (WriterT)

class Monad m => MonadStates s m where
    get :: m s
    put :: s -> m ()

instance Monad m => MonadStates s (StateT s m) where
    get = MTL.get
    put = MTL.put

instance MonadStates s m => MonadStates s (ReaderT r m) where
  get = lift get
  put = lift . put

instance (Monoid w, MonadStates s m) => MonadStates s (WriterT w m) where
  get = lift get
  put = lift . put

modify' :: (Monad m, MonadStates s m) => (s -> s) -> m ()
modify' f = get >>= put . f

use' :: forall s m a. (Monad m, MonadStates s m) => Getting a s a -> m a
use' lns = view lns <$> (get :: m s)

-- | Modify part of the s
over' :: forall s m a. (Monad m, MonadStates s m) => ASetter s s a a -> (a -> a) -> m ()
over' lns f = get >>= put . over lns f {->> get-}
