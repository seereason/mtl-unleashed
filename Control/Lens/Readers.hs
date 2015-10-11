-- | Versions of the lens operators for MonadStates instead of MonadState.
module Control.Lens.Readers
    ( module Control.Monad.Readers
    , asks
    , views
    , iview
    , iviews
    , review
    , view
    , reviews
    , preview
    , ipreview
    , previews
    ) where

import Control.Lens hiding (views, iview, iviews, review, view, reviews, preview, ipreview, previews)
import Control.Monad.Readers
import Control.Monad.Writer (First(First, getFirst))
import Data.Monoid -- for base < 4.8
import Data.Profunctor.Unsafe ((#.), (.#))
import Data.Tagged (Tagged(Tagged, unTagged))

asks :: MonadReaders r m => (r -> a) -> m a
asks = readerPoly

views :: MonadReaders s m => LensLike' (Const r) s a -> (a -> r) -> m r
views l f = asks (getConst #. l (Const #. f))
{-# INLINE views #-}

iview :: MonadReaders s m => IndexedGetting i (i,a) s a -> m (i,a)
iview l = asks (getConst #. l (Indexed $ \i -> Const #. (,) i))
{-# INLINE iview #-}

iviews :: MonadReaders s m => IndexedGetting i r s a -> (i -> a -> r) -> m r
iviews l f = asks (getConst #. l (Const #. Indexed f))
{-# INLINE iviews #-}

review :: MonadReaders b m => AReview t b -> m t
review p = asks (runIdentity #. unTagged #. p .# Tagged .# Identity)
{-# INLINE review #-}

view :: MonadReaders r m => Getting a r a -> m a
view = viewPoly
{-# INLINE view #-}

reviews :: MonadReaders b m => AReview t b -> (t -> r) -> m r
reviews p tr = asks (tr . runIdentity #. unTagged #. p .# Tagged .# Identity)
{-# INLINE reviews #-}

preview :: MonadReaders s m => Getting (First a) s a -> m (Maybe a)
preview l = asks (getFirst #. foldMapOf l (First #. Just))
{-# INLINE preview #-}

ipreview :: MonadReaders s m => IndexedGetting i (First (i, a)) s a -> m (Maybe (i, a))
ipreview l = asks (getFirst #. ifoldMapOf l (\i a -> First (Just (i, a))))
{-# INLINE ipreview #-}

previews :: MonadReaders s m => Getting (First r) s a -> (a -> r) -> m (Maybe r)
previews l f = asks (getFirst . foldMapOf l (First #. Just . f))
{-# INLINE previews #-}
