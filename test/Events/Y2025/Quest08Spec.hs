module Events.Y2025.Quest08Spec (spec) where

import Events.Y2025.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 8" $ do
  it "Sample" $ do
    s <- quest08 "sample"
    s `shouldBe` (4, 21, 7)

  it "Actual" $ do
    a <- quest08 "actual"
    a `shouldBe` (53, 2928460, 2800)
