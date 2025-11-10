module Day03Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 3" $ do
  it "Sample" $ do
    s <- day03 "sample"
    s `shouldBe` (29, 781, 3)

  it "Actual" $ do
    a <- day03 "actual"
    a `shouldBe` (2633, 297, 4485)
