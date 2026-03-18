module Stories.S1.Quest01Spec (spec) where

import SpecHelper
import Stories.S1.Quests

spec :: Spec
spec = describe "Story 1, Quest 1" $ do
  it "Sample" $ do
    s <- quest01 "sample"
    s `shouldBe` (11611972920, 1507702060886, 3279640)

  it "Actual" $ do
    a <- quest01 "actual"
    a `shouldBe` (1000171842, 163614275874495, 567008267427112)
