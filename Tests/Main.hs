module Main where

import Control.Lens
import Control.Monad.Reader (runReader)
import Control.Monad.State (evalState)
import Test.Hspec hiding (runIO)
import Test.Hspec.Core.Spec (SpecM)
import Types

main :: IO ()
main = hspec $ tests

tests :: SpecM () ()
tests = do
  it "runs a test" $ do
     1 `shouldBe` 1

  it "sees some state" $ do
     evalState (use foo) (T 5 'r') `shouldBe` 5

  it "sees T using MonadReaders class and lens" $ do
     runReader monadReaders1 (T 5 'r') `shouldBe` 5

  it "sees T using MonadReaders class and modified lens" $ do
     runReader monadReaders2 (T 5 'r') `shouldBe` 5

  it "sees T using MonadStates class" $ do
     evalState monadStates1 (T 5 'r') `shouldBe` 5
