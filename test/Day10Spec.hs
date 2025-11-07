module Day10Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 10" $ do
  it "Sample" $ do
    s <- day10 "sample"
    s `shouldBe` (0, 0, 0)

  it "Actual" $ do
    a <- day10 "actual"
    a `shouldBe` (0, 0, 0)