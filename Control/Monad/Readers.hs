-- | MonadState without the function dependency @m -> s@.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Readers
    ( module Control.Monad.Reader
    , MonadReaders(ask, local)
    , asks
    ) where

import Control.Monad.Reader hiding (MonadReader(ask, local, reader), asks)
import qualified Control.Monad.Reader as MTL (ask, local)
import Control.Monad.State (StateT, mapStateT)
import Control.Monad.Writer (WriterT, mapWriterT)

-- | Version of MonadReader modified to remove the functional dependency.
class Monad m => MonadReaders r m where
    ask :: m r
    ask = reader id

    local :: (r -> r) -> m a -> m a

    reader :: (r -> a) -> m a
    reader f = do
      r <- ask
      return (f r)

instance Monad m => MonadReaders r (ReaderT r m) where
    ask = MTL.ask
    local = MTL.local

instance (Monad m, MonadReaders r m) => MonadReaders r (StateT s m) where
    ask = lift ask
    local = mapStateT . local

instance (Monad m, Monoid w, MonadReaders r m) => MonadReaders r (WriterT w m) where
    ask = lift ask
    local = mapWriterT . local

{-
-- Here is how you create a MonadReaders instance for a type that is
-- nested inside another MonadReaders instances.   You need to declare
-- this with the exact parent type (here Bar) or you will get
-- overlapping instances.

instance (Monad m, MonadReaders Foo m) => MonadReaders Foo (ReaderT Bar m) where
    ask = lift ask
    local f action = MTL.ask >>= runReaderT (local f (lift action))
-}

-- | Retrieves a function of the current environment.
asks :: MonadReaders r m
    => (r -> a) -- ^ The selector function to apply to the environment.
    -> m a
asks = reader
