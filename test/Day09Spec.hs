module Day09Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 9" $ do
  it "Sample" $ do
    s <- day09 "sample"
    s `shouldBe` (0, 0, 0)

  it "Actual" $ do
    a <- day09 "actual"
    a `shouldBe` (0, 0, 0)