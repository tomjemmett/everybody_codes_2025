module Stories.S2.Quest01Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Story 2, Quest 1" $ do
  it "Sample" $ do
    s <- s2q1 "sample"
    s `shouldBe` (26, 115, "13 43")

  it "Actual" $ do
    a <- s2q1 "actual"
    a `shouldBe` (50, 1130, "25 110")
