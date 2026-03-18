module Stories.S2.Quest01Spec (spec) where

import SpecHelper
import Stories.S2.Quests

spec :: Spec
spec = describe "Story 2, Quest 1" $ do
  it "Sample" $ do
    s <- quest01 "sample"
    s `shouldBe` (26, 115, "13 43")

  it "Actual" $ do
    a <- quest01 "actual"
    a `shouldBe` (50, 1130, "25 110")
