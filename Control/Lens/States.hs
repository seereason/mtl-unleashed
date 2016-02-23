{-# LANGUAGE CPP #-}
-- | Versions of the lens operators for MonadStates instead of MonadState.
module Control.Lens.States
    ( module Control.Monad.States
    , use
    , get
    , put
    , (.=)
    , (%=)
#ifdef MIN_VERSION_lens
    , (?=)
#endif
    , (<~)
    , (<.=)
#ifdef MIN_VERSION_lens
    , (<?=)
#endif
    , (<>~)
    ) where

import Control.Applicative -- for base < 4.8
import Control.Lens hiding (use, (.=), (%=), (?=), (<~), (<.=), (<?=), (<>~))
import Control.Monad.States
import Data.Monoid -- for base < 4.8

use :: MonadStates s m => Getting a s a -> m a
use = usePoly

get :: MonadStates s m => m s
get = getPoly

put :: MonadStates s m => s -> m ()
put = putPoly

(.=) :: MonadStates s m => ASetter s s a b -> b -> m ()
l .= b = modifyPoly (l .~ b)
{-# INLINE (.=) #-}

(%=) :: MonadStates s m => ASetter s s a b -> (a -> b) -> m ()
l %= f = modifyPoly (l %~ f)
{-# INLINE (%=) #-}

#ifdef MIN_VERSION_lens
?~ is not available in microlens
(?=) :: MonadStates s m => ASetter s s a (Maybe b) -> b -> m ()
l ?= b = modifyPoly (l ?~ b)
{-# INLINE (?=) #-}
#endif

(<~) :: MonadStates s m => ASetter s s a b -> m b -> m ()
l <~ mb = mb >>= (l .=)
{-# INLINE (<~) #-}

(<.=) :: MonadStates s m => ASetter s s a b -> b -> m b
l <.= b = do
  l .= b
  return b
{-# INLINE (<.=) #-}

#ifdef MIN_VERSION_lens
(<?=) :: MonadStates s m => ASetter s s a (Maybe b) -> b -> m b
l <?= b = do
  l ?= b
  return b
{-# INLINE (<?=) #-}
#endif

(<>~) :: Monoid a => ASetter s t a a -> a -> s -> t
l <>~ n = over l (`mappend` n)
{-# INLINE (<>~) #-}
