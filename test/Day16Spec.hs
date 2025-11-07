module Day16Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 16" $ do
  it "Sample" $ do
    s <- day16 "sample"
    s `shouldBe` (0, 0, 0)

  it "Actual" $ do
    a <- day16 "actual"
    a `shouldBe` (0, 0, 0)