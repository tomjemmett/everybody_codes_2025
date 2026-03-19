module Events.Y2024.Quest13Spec (spec) where

import Events.Y2024.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 13" $ do
  it "Sample" $ do
    s <- quest13 "sample"
    s `shouldBe` (28, 28, 14)

  it "Actual" $ do
    a <- quest13 "actual"
    a `shouldBe` (149, 574, 555)
