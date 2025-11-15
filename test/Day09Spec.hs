module Day09Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 9" $ do
  it "Sample" $ do
    s <- day09 "sample"
    s `shouldBe` (414, 1245, 12)

  it "Actual" $ do
    a <- day09 "actual"
    a `shouldBe` (5776, 317847, 37926)
