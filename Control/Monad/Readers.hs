-- | MonadState without the function dependency @m -> s@.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Readers
    ( module Control.Monad.Reader
    , magnify'
    , MonadReaders(ask, local)
    , asks
    , view, views, iview, iviews
    , preview, previews, ipreview, ipreviews
    , review, reviews
    , Magnify(magnify)
    ) where

import Control.Lens as Lens hiding (view, iview, views, uses, iviews, preview, previews, ipreview, ipreviews, review, reviews, Magnify)
import Control.Lens.Internal.Zoom (Magnified)
import Control.Monad.Reader hiding (MonadReader(ask, local, reader), asks)
import qualified Control.Monad.Reader as MTL (ask, local)
import Control.Monad.State (mapStateT, StateT)
import Control.Monad.Writer (WriterT, mapWriterT)
import Data.Monoid
import Data.Profunctor.Unsafe
import Data.Tagged

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

view :: MonadReaders s m => Getting a s a -> m a
view l = Control.Monad.Readers.asks (getConst #. l Const)

views :: (Profunctor p, MonadReaders s m) => Optical p (->) (Const r) s s a a -> p a r -> m r
views l f = Control.Monad.Readers.asks (getConst #. l (Const #. f))

iview :: MonadReaders s m => IndexedGetting i (i,a) s a -> m (i,a)
iview l = Control.Monad.Readers.asks (getConst #. l (Indexed $ \i -> Const #. (,) i))

iviews :: MonadReaders s m => IndexedGetting i r s a -> (i -> a -> r) -> m r
iviews l = views l .# Indexed

review :: MonadReaders b m => AReview t b -> m t
review p = asks (runIdentity #. unTagged #. p .# Tagged .# Identity)

reviews :: MonadReaders b m => AReview t b -> (t -> r) -> m r
reviews p tr = asks (tr . runIdentity #. unTagged #. p .# Tagged .# Identity)

instance MonadReaders s (ReifiedGetter s) where
  ask = Getter id
  {-# INLINE ask #-}
  local f m = Getter (to f . runGetter m)
  {-# INLINE local #-}

instance MonadReaders s (ReifiedFold s) where
  ask = Fold id
  {-# INLINE ask #-}
  local f m = Fold (to f . runFold m)
  {-# INLINE local #-}

preview :: MonadReaders s m => Getting (First a) s a -> m (Maybe a)
preview l = asks (getFirst #. foldMapOf l (First #. Just))

ipreview :: MonadReaders s m => IndexedGetting i (First (i, a)) s a -> m (Maybe (i, a))
ipreview l = asks (getFirst #. ifoldMapOf l (\i a -> First (Just (i, a))))

previews :: MonadReaders s m => Getting (First r) s a -> (a -> r) -> m (Maybe r)
previews l f = asks (getFirst . foldMapOf l (First #. Just . f))

ipreviews :: MonadReaders s m => IndexedGetting i (First r) s a -> (i -> a -> r) -> m (Maybe r)
ipreviews l f = asks (getFirst . ifoldMapOf l (\i -> First #. Just . f i))

class (Magnified m ~ Magnified n, MonadReaders b m, MonadReaders a n) => Magnify m n b a | m -> b, n -> a, m a -> n, n b -> m where
  magnify :: LensLike' (Magnified m c) a b -> m c -> n c

-- | I don't know why Control.Lens.magnify isn't working for me.
magnify' :: forall r s m b. MonadReaders s m => Getting r s r -> ReaderT r m b -> m b
magnify' lns action = view lns >>= runReaderT action
