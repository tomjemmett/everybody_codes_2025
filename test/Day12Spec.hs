module Day12Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 12" $ do
  it "Sample" $ do
    s <- day12 "sample"
    s `shouldBe` (16, 58, 14)

  it "Actual" $ do
    a <- day12 "actual"
    a `shouldBe` (242, 5678, 4098)
