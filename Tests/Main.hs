{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Lens as Lens
import Control.Monad.Reader (ReaderT, runReader, runReaderT)
import Control.Monad.Readers as Readers
import Control.Monad.State (evalState)
import Control.Monad.States as States
import qualified Control.Monad.Reader as MTL
import qualified Control.Monad.State as MTL
import Test.Hspec hiding (runIO)
import Test.Hspec.Core.Spec (SpecM)

data S = S {
      _s1 :: Float,
      _s2 :: Bool
    }

data T = T {
      _t1 :: Int,
      _t2 :: Char
    }

data R = R {
      _s :: S
    , _t :: T
    }

$(makeLenses ''S)
$(makeLenses ''T)
$(makeLenses ''R)

runBoth action = runReader (runReaderT action (T 5 'x')) (S 2.5 True)

runR action = runReader action (R (S 2.5 True) (T 5 'x'))

instance Monad m => MonadReaders S (ReaderT R m) where
    ask = Lens.magnify s ask
    local f action = ask >>= \(r :: R) -> runReaderT (MTL.lift action) (set s (f (Lens.view s r)) r)

instance Monad m => MonadReaders T (ReaderT R m) where
    ask = Lens.magnify t ask
    local f action = ask >>= \(r :: R) -> runReaderT (MTL.lift action) (set t (f (Lens.view t r)) r)

instance (MonadReaders S m, Monad m) => MonadReaders S (ReaderT T m) where
    ask = MTL.lift ask
    local f action = MTL.ask >>= MTL.runReaderT (local f (MTL.lift action))

-- This is where the advantages of MonadReaders start to appear:
-- use two different MonadReaders instances for the same monad.
s1t1 :: forall m. (Applicative m, Functor m, MonadReaders S m, MonadReaders T m) => m (Float, Int)
-- s1t1 = (,) <$> Readers.view s1 <*> Readers.view t1
s1t1 = (,) <$> (view s1 <$> (ask :: m S)) <*> (view t1 <$> (ask :: m T))

tests :: SpecM () ()
tests = do
  it "runs a test" $ do
     1 `shouldBe` 1

  it "Can read a value out of Reader using Lens.view" $ do
     runReader (Lens.view t1) (T 3 'x') `shouldBe` 3

  it "Can read a value out of Reader using Readers.view" $ do
     runReader (Lens.view t1) (T 3 'x') `shouldBe` 3

  it "Can read a pure value using Lens.view" $ do
     Lens.view t1 (T 3 'x') `shouldBe` 3

#if 0
  -- Something that makes this work for Lens is missing.  I have a
  -- feeling inserting a type signature somewhere would help.
  it "Can read a pure value Readers.view" $ do
     Readers.view t1 (T 3 'x') `shouldBe` (3 :: Int)
#endif

  it "sees T using MonadReaders class and modified lens" $ do
     runReader (view t1) (T 3 'x') `shouldBe` 3

  it "Can use state using Lens" $ do
     evalState (Lens.use t1) (T 5 'r') `shouldBe` 5

  it "Can use state using States" $ do
     evalState (use t1) (T 3 'x') `shouldBe` 3
#if 0
  -- These two won't compile due to functional dependency
  it "sees T using MonadReader class and lens" $ do
     runBoth (Lens.view t1) `shouldBe` 5

  it "sees S using MonadReader class and lens" $ do
     runBoth (Lens.view s1) `shouldBe` 2.5
#endif

  it "sees T using MonadReaders class and lens" $ do
     runBoth (view' t1) `shouldBe` 5

  it "sees S using MonadReaders class and lens" $ do
     runBoth (view' s1) `shouldBe` 2.5

  it "Can read two different types out of the same monad" $ do
     runR s1t1 `shouldBe` (2.5, 5)

main :: IO ()
main = hspec $ tests
