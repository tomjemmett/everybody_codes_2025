module Day13Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 13" $ do
  it "Sample" $ do
    s <- day13 "sample"
    s `shouldBe` (0, 0, 0)

  it "Actual" $ do
    a <- day13 "actual"
    a `shouldBe` (0, 0, 0)
