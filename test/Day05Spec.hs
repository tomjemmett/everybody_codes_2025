module Day05Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 5" $ do
  it "Sample" $ do
    s <- day05 "sample"
    s `shouldBe` (0, 0, 0)

  it "Actual" $ do
    a <- day05 "actual"
    a `shouldBe` (0, 0, 0)