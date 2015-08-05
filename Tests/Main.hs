module Main where

import Test.Hspec hiding (runIO)
import Test.Hspec.Core.Spec (SpecM)
import Types

main :: IO ()
main = hspec $ tests

tests :: SpecM () ()
tests = do
  it "runs a test" $ do
     1 `shouldBe` 1

