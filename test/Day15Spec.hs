module Day15Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 15" $ do
  it "Sample" $ do
    s <- day15 "sample"
    s `shouldBe` (0, 0, 0)

  it "Actual" $ do
    a <- day15 "actual"
    a `shouldBe` (0, 0, 0)
