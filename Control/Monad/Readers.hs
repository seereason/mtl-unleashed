-- | MonadState without the function dependency @m -> s@.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Readers
    ( module Control.Monad.Reader
    , MonadReaders(ask, local)
    , view'
    ) where

import Control.Lens
import Control.Monad.Reader hiding (ask, local)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import qualified Control.Monad.Reader as MTL (ask, local)

class Monad m => MonadReaders r m where
  ask :: m r
  local :: (r -> r) -> m a -> m a

instance Monad m => MonadReaders r (ReaderT r m) where
  ask = MTL.ask
  local f = MTL.local f

instance MonadReaders r m => MonadReaders r (StateT s m) where
  ask = lift ask
  local f action = local f action

instance (Monoid w, MonadReaders r m) => MonadReaders r (WriterT w m) where
  ask = lift ask
  local f action = local f action

-- | Modified view function that works if there is a MonadReaders r instance
view' :: forall r m a. (Monad m, MonadReaders r m) => Getting a r a -> m a
view' lns = view lns <$> (ask :: m r)
