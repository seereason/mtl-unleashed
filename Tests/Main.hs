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
import Control.Monad.State (evalState, lift)
import Control.Monad.States as States
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

-- Custom instances to direct us through stacked/nested reader monads -
-- how to get an S from an R
instance Monad m => MonadReaders S (ReaderT R m) where
    askPoly = Lens.magnify s askPoly
    localPoly f action = askPoly >>= \(r :: R) -> runReaderT (lift action) (set s (f (Lens.view s r)) r)

-- how to get a T from an R
instance Monad m => MonadReaders T (ReaderT R m) where
    askPoly = Lens.magnify t askPoly
    localPoly f action = askPoly >>= \(r :: R) -> runReaderT (lift action) (set t (f (Lens.view t r)) r)

-- how to get an S from a T (it doesn't have one, we have
-- to get it from m.)
instance MonadReaders S m => MonadReaders S (ReaderT T m) where
    askPoly = lift askPoly
    localPoly f action = lift (askPoly :: m S) >>= runReaderT (localPoly f (lift action))

tests :: SpecM () ()
tests = do
  it "runs a test" $ do
     1 `shouldBe` 1

  it "Can read a value out of Reader using Lens.view" $ do
     runReader (view t1) (T 3 'x') `shouldBe` 3

  it "Can read a value out of Reader using Readers.viewPoly" $ do
     runReader (viewPoly t1) (T 3 'x') `shouldBe` 3

  it "Can read a pure value using Lens.view" $ do
     Lens.view t1 (T 3 'x') `shouldBe` 3

  it "Can read a pure value Readers.viewPoly" $ do
     Readers.viewPoly t1 (T 3 'x') `shouldBe` (3 :: Int)

  it "sees T using MonadReaders class and modified lens" $ do
     runReader (viewPoly t1) (T 3 'x') `shouldBe` 3

  it "Can use state using Lens" $ do
     evalState (Lens.use t1) (T 5 'r') `shouldBe` 5

  it "Can use state using States" $ do
     evalState (usePoly t1) (T 3 'x') `shouldBe` 3
#if 0
  -- These two won't compile due to the functional dependency
  -- this package is designed to circumvent.
  it "sees T using MonadReader class and lens" $ do
     runBoth (Lens.view t1) `shouldBe` 5

  it "sees S using MonadReader class and lens" $ do
     runBoth (Lens.view s1) `shouldBe` 2.5
#endif
  -- These two show how viewPoly wins over view.
  it "sees T using MonadReaders class and lens" $ do
     runBoth (viewPoly t1) `shouldBe` 5

  it "sees S using MonadReaders class and lens" $ do
     runBoth (viewPoly s1) `shouldBe` 2.5

  it "Can read two different types out of the same monad" $ do
     runR s1t1 `shouldBe` (2.5, 5)

-- This is where the advantages of MonadReaders start to appear: use
-- multiple MonadReaders instances in the signature context for the
-- same monad.
s1t1 :: forall m. (Applicative m, Functor m, MonadReaders S m, MonadReaders T m) => m (Float, Int)
s1t1 = (,) <$> viewPoly s1 <*> viewPoly t1

main :: IO ()
main = hspec $ tests
