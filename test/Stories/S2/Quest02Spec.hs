module Stories.S2.Quest02Spec (spec) where

import SpecHelper
import Stories.S2.Quests

spec :: Spec
spec = describe "Story 2, Quest 2" $ do
  it "Sample" $ do
    s <- quest02 "sample"
    s `shouldBe` (7, 2955, 2953681)

  it "Actual" $ do
    a <- quest02 "actual"
    a `shouldBe` (131, 21497, 21199950)
