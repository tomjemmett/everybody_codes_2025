module Events.Y2025.Quest14Spec (spec) where

import Events.Y2025.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 14" $ do
  it "Sample" $ do
    s <- quest14 "sample"
    s `shouldBe` (200, 39349, 278388552)

  it "Actual" $ do
    a <- quest14 "actual"
    a `shouldBe` (489, 1169832, 1011964800)
