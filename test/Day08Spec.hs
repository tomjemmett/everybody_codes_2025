module Day08Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 8" $ do
  it "Sample" $ do
    s <- day08 "sample"
    s `shouldBe` (4, 21, 7)

  it "Actual" $ do
    a <- day08 "actual"
    a `shouldBe` (53, 2928640, 2800)
