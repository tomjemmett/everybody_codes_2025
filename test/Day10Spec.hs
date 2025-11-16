module Day10Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 10" $ do
  it "Sample" $ do
    s <- day10 "sample"
    s `shouldBe` (27, 27, 13033988838)

  it "Actual" $ do
    a <- day10 "actual"
    a `shouldBe` (143, 1749, 33411392898387)
