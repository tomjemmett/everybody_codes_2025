module Events.Y2024.Quest08Spec (spec) where

import Events.Y2024.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 8" $ do
  it "Sample" $ do
    s <- quest08 "sample"
    s `shouldBe` (21, 27, 2)

  it "Actual" $ do
    a <- quest08 "actual"
    a `shouldBe` (9725698, 154255165, 41082)
