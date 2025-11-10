module Day14Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 14" $ do
  it "Sample" $ do
    s <- day14 "sample"
    s `shouldBe` (0, 0, 0)

  it "Actual" $ do
    a <- day14 "actual"
    a `shouldBe` (0, 0, 0)
