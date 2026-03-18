module Events.Y2025.Quest13Spec (spec) where

import Events.Y2025.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 13" $ do
  it "Sample" $ do
    s <- quest13 "sample"
    s `shouldBe` (67, 30, 30)

  it "Actual" $ do
    a <- quest13 "actual"
    a `shouldBe` (541, 8578, 551167)
