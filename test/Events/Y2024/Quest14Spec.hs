module Events.Y2024.Quest14Spec (spec) where

import Events.Y2024.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 14" $ do
  it "Sample" $ do
    s <- quest14 "sample"
    s `shouldBe` (7, 32, 46)

  it "Actual" $ do
    a <- quest14 "actual"
    a `shouldBe` (152, 5008, 1454)
