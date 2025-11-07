module Day08Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 8" $ do
  it "Sample" $ do
    s <- day08 "sample"
    s `shouldBe` (0, 0, 0)

  it "Actual" $ do
    a <- day08 "actual"
    a `shouldBe` (0, 0, 0)