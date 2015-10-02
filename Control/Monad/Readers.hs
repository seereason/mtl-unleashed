-- | MonadState without the function dependency @m -> s@.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Readers
    ( module Control.Monad.Trans.Reader
    , MonadReaders(askPoly, localPoly)
    , viewPoly
    ) where

import Control.Lens
--import Control.Monad.Reader
--import Control.Monad.State (mapStateT, StateT)
--import Control.Monad.Writer (mapWriterT, WriterT)


import Control.Monad.Trans.Cont as Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Error
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as ReaderT (ask, local, reader)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST, ask, local, reader)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST, ask, local, reader)
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict

import Control.Monad.Trans.Class (lift)
-- import Control.Monad
-- import Data.Monoid

class Monad m => MonadReaders r m where
  askPoly :: m r
  localPoly :: (r -> r) -> m a -> m a
  -- | Retrieves a function of the current environment.
  readerPoly :: (r -> a) -- ^ The selector function to apply to the environment.
             -> m a
  readerPoly f = do
      r <- askPoly :: m r
      return (f r)

-- | Modified view function that works if there is a MonadReaders r instance
viewPoly :: forall r m a. MonadReaders r m => Getting a r a -> m a
viewPoly lns = (askPoly :: m r) >>= return . view lns

instance Monad m => MonadReaders r (ReaderT r m) where
  askPoly = ReaderT.ask
  localPoly = ReaderT.local
  readerPoly = ReaderT.reader

instance MonadReaders r ((->) r) where
    askPoly       = id
    localPoly f m = m . f
    readerPoly    = id

instance (Monad m, Monoid w) => MonadReaders r (LazyRWS.RWST r w s m) where
    askPoly = LazyRWS.ask
    localPoly = LazyRWS.local
    readerPoly = LazyRWS.reader

instance (Monad m, Monoid w) => MonadReaders r (StrictRWS.RWST r w s m) where
    askPoly = StrictRWS.ask
    localPoly = StrictRWS.local
    readerPoly = StrictRWS.reader

{-
instance MonadReaders r m => MonadReaders r (StateT s m) where
  askPoly = lift askPoly
  localPoly = mapStateT . localPoly

instance (Monoid w, MonadReaders r m) => MonadReaders r (WriterT w m) where
  askPoly = lift askPoly
  localPoly = mapWriterT . localPoly
-}

-- | Modiifed copies of instances from mtl.
instance MonadReaders r' m => MonadReaders r' (ContT r m) where
    askPoly   = lift askPoly
    localPoly = Cont.liftLocal askPoly localPoly
    readerPoly = lift . readerPoly

instance (Error e, MonadReaders r m) => MonadReaders r (ErrorT e m) where
    askPoly   = lift askPoly
    localPoly = mapErrorT . localPoly
    readerPoly = lift . readerPoly

instance MonadReaders r m => MonadReaders r (ExceptT e m) where
    askPoly   = lift askPoly
    localPoly = mapExceptT . localPoly
    readerPoly = lift . readerPoly

instance MonadReaders r m => MonadReaders r (IdentityT m) where
    askPoly   = lift askPoly
    localPoly = mapIdentityT . localPoly
    readerPoly = lift . readerPoly

instance MonadReaders r m => MonadReaders r (ListT m) where
    askPoly   = lift askPoly
    localPoly = mapListT . localPoly
    readerPoly = lift . readerPoly

instance MonadReaders r m => MonadReaders r (MaybeT m) where
    askPoly   = lift askPoly
    localPoly = mapMaybeT . localPoly
    readerPoly = lift . readerPoly

instance MonadReaders r m => MonadReaders r (Lazy.StateT s m) where
    askPoly   = lift askPoly
    localPoly = Lazy.mapStateT . localPoly
    readerPoly = lift . readerPoly

instance MonadReaders r m => MonadReaders r (Strict.StateT s m) where
    askPoly   = lift askPoly
    localPoly = Strict.mapStateT . localPoly
    readerPoly = lift . readerPoly

instance (Monoid w, MonadReaders r m) => MonadReaders r (Lazy.WriterT w m) where
    askPoly   = lift askPoly
    localPoly = Lazy.mapWriterT . localPoly
    readerPoly = lift . readerPoly

instance (Monoid w, MonadReaders r m) => MonadReaders r (Strict.WriterT w m) where
    askPoly   = lift askPoly
    localPoly = Strict.mapWriterT . localPoly
    readerPoly = lift . readerPoly
