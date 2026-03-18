module Events.Y2025.Quest19Spec (spec) where

import Events.Y2025.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 19" $ do
  it "Sample" $ do
    s <- quest19 "sample"
    s `shouldBe` (24, 22, 22)

  it "Actual" $ do
    a <- quest19 "actual"
    a `shouldBe` (55, 735, 4558484)
