module Stories.S1.Quest03Spec (spec) where

import SpecHelper
import Stories.S1.Quests

spec :: Spec
spec = describe "Story 1, Quest 3" $ do
  it "Sample" $ do
    s <- quest03 "sample"
    s `shouldBe` (1310, 13659, 13659)

  it "Actual" $ do
    a <- quest03 "actual"
    a `shouldBe` (3444, 1029790, 90312279547)
