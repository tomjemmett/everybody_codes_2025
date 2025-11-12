module Day07Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 7" $ do
  it "Sample" $ do
    s <- day07 "sample"
    s `shouldBe` ("Oroneth", 23, 1154)

  it "Actual" $ do
    a <- day07 "actual"
    a `shouldBe` ("Nyjorath", 3055, 9416786)
