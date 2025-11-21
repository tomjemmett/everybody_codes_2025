module Day14Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 14" $ do
  it "Sample" $ do
    s <- day14 "sample"
    s `shouldBe` (200, 39349, 278388552)

  it "Actual" $ do
    a <- day14 "actual"
    a `shouldBe` (489, 1169832, 1011964800)
