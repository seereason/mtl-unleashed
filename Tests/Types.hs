{-# LANGUAGE NoPolyKinds #-}
{-# LANGUAGE NoDataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Types where

import Control.Lens hiding (view, use)
import qualified Control.Lens as Lens (view)
import qualified Control.Monad.Reader.Class as Reader
import Control.Monad.Readers as Readers
import Control.Monad.States as States

type Foo = Int
type Bar = Char

data T = T {
      _foo :: Foo,
      _bar :: Bar
    }

$(makeLenses ''T)

instance Monad m => MonadStates Foo (StateT T m) where
    get = use foo
    put s = foo .= s

instance Monad m => MonadStates Bar (StateT T m) where
    get = use bar
    put s = bar .= s

instance Monad m => MonadReaders Foo (ReaderT T m) where
    ask = view foo
    local f action = Readers.ask >>= \(t :: T) -> runReaderT (lift action) (set foo (f (Lens.view foo t)) t)

instance Monad m => MonadReaders Bar (ReaderT T m) where
    ask = view bar
    local f action = Readers.ask >>= \(t :: T) -> runReaderT (lift action) (set bar (f (Lens.view bar t)) t)

monadReaders1 :: MonadReaders T m => m Foo
monadReaders1 = Readers.ask >>= \(t :: T) -> return $ Lens.view foo t

monadReaders2 :: MonadReaders T m => m Foo
monadReaders2 = view foo

monadReaders3 :: Reader.MonadReader T m => m Foo
monadReaders3 = Lens.view foo

monadStates1 :: MonadStates T m => m Foo
monadStates1 = get >>= \(t :: T) -> return $ Lens.view foo t

