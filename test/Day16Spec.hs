module Day16Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 16" $ do
  it "Sample" $ do
    s <- day16 "sample"
    s `shouldBe` (193, 270, 94439495762954)

  it "Actual" $ do
    a <- day16 "actual"
    a `shouldBe` (207, 112513370112, 93567233166974)
