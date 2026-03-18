module Events.Y2025.Quest07Spec (spec) where

import Events.Y2025.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 7" $ do
  it "Sample" $ do
    s <- quest07 "sample"
    s `shouldBe` ("Oroneth", 23, 1154)

  it "Actual" $ do
    a <- quest07 "actual"
    a `shouldBe` ("Nyjorath", 3055, 9416786)
