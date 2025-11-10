module Day20Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 20" $ do
  it "Sample" $ do
    s <- day20 "sample"
    s `shouldBe` (0, 0, 0)

  it "Actual" $ do
    a <- day20 "actual"
    a `shouldBe` (0, 0, 0)
