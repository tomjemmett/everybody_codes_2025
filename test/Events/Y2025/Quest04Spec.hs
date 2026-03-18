module Events.Y2025.Quest04Spec (spec) where

import Events.Y2025.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 4" $ do
  it "Sample" $ do
    s <- quest04 "sample"
    s `shouldBe` (15888, 1274509803922, 6818)

  it "Actual" $ do
    a <- quest04 "actual"
    a `shouldBe` (14260, 2878635907724, 878468933346)
