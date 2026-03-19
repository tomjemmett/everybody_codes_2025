module Events.Y2024.Quest06Spec (spec) where

import Events.Y2024.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 6" $ do
  it "Sample" $ do
    s <- quest06 "sample"
    s `shouldBe` ("RRB@", "RB@", "RB@")

  it "Actual" $ do
    a <- quest06 "actual"
    a `shouldBe` ("RRWQXZZHGSMN@", "RPHCSQFJLB@", "RWGFCJBVGMVC@")
