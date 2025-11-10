module Day17Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 17" $ do
  it "Sample" $ do
    s <- day17 "sample"
    s `shouldBe` (0, 0, 0)

  it "Actual" $ do
    a <- day17 "actual"
    a `shouldBe` (0, 0, 0)
