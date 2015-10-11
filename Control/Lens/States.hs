-- | Versions of the lens operators for MonadStates instead of MonadState.
module Control.Lens.States
    ( module Control.Monad.States
    , use
    , get
    , put
    , (.=)
    , (%=)
    , (?=)
    , (<~)
    , (<.=)
    , (<?=)
    , (<>~)
    ) where

import Control.Lens hiding (use, (.=), (%=), (?=), (<~), (<.=), (<?=), (<>~))
import Control.Monad.States hiding (get, put)

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

(?=) :: MonadStates s m => ASetter s s a (Maybe b) -> b -> m ()
l ?= b = modifyPoly (l ?~ b)
{-# INLINE (?=) #-}

(<~) :: MonadStates s m => ASetter s s a b -> m b -> m ()
l <~ mb = mb >>= (l .=)
{-# INLINE (<~) #-}

(<.=) :: MonadStates s m => ASetter s s a b -> b -> m b
l <.= b = do
  l .= b
  return b
{-# INLINE (<.=) #-}

(<?=) :: MonadStates s m => ASetter s s a (Maybe b) -> b -> m b
l <?= b = do
  l ?= b
  return b
{-# INLINE (<?=) #-}

(<>~) :: Monoid a => ASetter s t a a -> a -> s -> t
l <>~ n = over l (`mappend` n)
{-# INLINE (<>~) #-}
