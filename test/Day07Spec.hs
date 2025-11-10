module Day07Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 7" $ do
  it "Sample" $ do
    s <- day07 "sample"
    s `shouldBe` (0, 0, 0)

  it "Actual" $ do
    a <- day07 "actual"
    a `shouldBe` (0, 0, 0)
