module Day18Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 18" $ do
  it "Sample" $ do
    s <- day18 "sample"
    s `shouldBe` (774, 324, 946)

  it "Actual" $ do
    a <- day18 "actual"
    a `shouldBe` (2215338, 12081610449, 241487)
