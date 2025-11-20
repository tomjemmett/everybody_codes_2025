module Day13Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 13" $ do
  it "Sample" $ do
    s <- day13 "sample"
    s `shouldBe` (67,30,30)

  it "Actual" $ do
    a <- day13 "actual"
    a `shouldBe` (541, 8578, 551167)
