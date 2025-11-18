module Day11Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 11" $ do
  it "Sample" $ do
    s <- day11 "sample"
    s `shouldBe` (109, 1579, 1378)

  it "Actual" $ do
    a <- day11 "actual"
    a `shouldBe` (269, 3090294, 135000728971927)
