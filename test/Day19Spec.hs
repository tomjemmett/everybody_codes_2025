module Day19Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 19" $ do
  it "Sample" $ do
    s <- day19 "sample"
    s `shouldBe` (24, 22, 22)

  it "Actual" $ do
    a <- day19 "actual"
    a `shouldBe` (55, 735, 4558484)
