module Stories.S2.Quest03Spec (spec) where

import SpecHelper
import Stories.S2.Quests

spec :: Spec
spec = describe "Story 2, Quest 3" $ do
  it "Sample" $ do
    s <- quest03 "sample"
    s `shouldBe` (844, "1,3,4,2", 1125)

  it "Actual" $ do
    a <- quest03 "actual"
    a `shouldBe` (622, "9,2,8,6,3,7,5,4,1", 154248)
