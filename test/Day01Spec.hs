module Day01Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 1" $ do
  it "Sample" $ do
    s <- day01 "sample"
    s `shouldBe` ("Fyrryn", "Elarzris", "Drakzyph")

  it "Actual" $ do
    a <- day01 "actual"
    a `shouldBe` ("Narcoryx", "Dorxith", "Uryth")