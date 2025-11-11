module Day06Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 6" $ do
  it "Sample" $ do
    s <- day06 "sample"
    s `shouldBe` (5, 11, 3442321)

  it "Actual" $ do
    a <- day06 "actual"
    a `shouldBe` (137, 4223, 1665521085)