-- | MonadState without the function dependency @m -> s@.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Reader.Extra
    ( module Control.Monad.Reader
    , MonadReader(ask, local)
    , asks
    ) where

import Control.Monad.Reader hiding (MonadReader, ask, local, reader, asks)
import qualified Control.Monad.Reader as M (ask, local)
import Control.Monad.State (StateT, get, evalStateT)
import Control.Monad.Writer (runWriterT, WriterT)

-- | Version of MonadReader modified to remove the functional dependency.
class Monad m => MonadReader r m where
    ask :: m r
    ask = reader id

    local :: (r -> r) -> m a -> m a

    reader :: (r -> a) -> m a
    reader f = do
      r <- ask
      return (f r)

instance Monad m => MonadReader r (ReaderT r m) where
    ask = M.ask
    local = M.local

instance (Monad m, MonadReader r m) => MonadReader r (StateT s m) where
    ask = lift ask
    local f action = get >>= lift . local f . evalStateT action

instance (Monad m, Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
    ask = lift ask
    local f action = lift (local f (runWriterT action >>= return . fst))

{-
-- Here is how you create a MonadReader instance for a type that is
-- nested inside another MonadReader instances.   You need to declare
-- this with the exact parent type (here Bar) or you will get
-- overlapping instances.

instance (Monad m, MonadReader Foo m) => MonadReader Foo (ReaderT Bar m) where
    ask = lift ask
    local f action = MTL.ask >>= runReaderT (local f (lift action))
-}

-- | Retrieves a function of the current environment.
asks :: MonadReader r m
    => (r -> a) -- ^ The selector function to apply to the environment.
    -> m a
asks = reader
