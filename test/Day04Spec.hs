module Day04Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 4" $ do
  it "Sample" $ do
    s <- day04 "sample"
    s `shouldBe` (15888, 1274509803922, 6818)

  it "Actual" $ do
    a <- day04 "actual"
    a `shouldBe` (14260, 2878635907724, 878468933346)