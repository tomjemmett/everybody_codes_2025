module Events.Y2024.Quest05Spec (spec) where

import Events.Y2024.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 5" $ do
  it "Sample" $ do
    s <- quest05 "sample"
    s `shouldBe` ("2323", "50877075", "6584")

  it "Actual" $ do
    a <- quest05 "actual"
    a `shouldBe` ("4245", "20502741815899", "8371100810001000")
