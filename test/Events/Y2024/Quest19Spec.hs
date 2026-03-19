module Events.Y2024.Quest19Spec (spec) where

import Events.Y2024.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 19" $ do
  it "Sample" $ do
    s <- quest19 "sample"
    s `shouldBe` ("WIN", "VICTORY", "-")

  it "Actual" $ do
    a <- quest19 "actual"
    a `shouldBe` ("7371449929834578", "3195919168998478", "-")
