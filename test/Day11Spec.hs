module Day11Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 11" $ do
  it "Sample" $ do
    s <- day11 "sample"
    s `shouldBe` (0, 0, 0)

  it "Actual" $ do
    a <- day11 "actual"
    a `shouldBe` (0, 0, 0)