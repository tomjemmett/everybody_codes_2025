module Day18Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 18" $ do
  it "Sample" $ do
    s <- day18 "sample"
    s `shouldBe` (0, 0, 0)

  it "Actual" $ do
    a <- day18 "actual"
    a `shouldBe` (0, 0, 0)