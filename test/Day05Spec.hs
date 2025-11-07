module Day05Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 5" $ do
  it "Sample" $ do
    s <- day05 "sample"
    s `shouldBe` (581078, 77053, 260)

  it "Actual" $ do
    a <- day05 "actual"
    a `shouldBe` (4563685626, 8617568255445, 31928850)