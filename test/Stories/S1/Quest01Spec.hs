module Stories.S1.Quest01Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Story 1, Quest 1" $ do
  it "Sample" $ do
    s <- s1q1 "sample"
    s `shouldBe` (11611972920, 1507702060886, 3279640)

  it "Actual" $ do
    a <- s1q1 "actual"
    a `shouldBe` (1000171842, 163614275874495, 567008267427112)
