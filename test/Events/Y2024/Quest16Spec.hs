module Events.Y2024.Quest16Spec (spec) where

import Events.Y2024.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 16" $ do
  it "Sample" $ do
    s <- quest16 "sample"
    s `shouldBe` (">.- -.- ^,-", 280014668134, "627 128")

  it "Actual" $ do
    a <- quest16 "actual"
    a `shouldBe` ("^:- -.> -.> -.>", 115605685111, "591 68")
