module Day12Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 12" $ do
  it "Sample" $ do
    s <- day12 "sample"
    s `shouldBe` (0, 0, 0)

  it "Actual" $ do
    a <- day12 "actual"
    a `shouldBe` (0, 0, 0)
