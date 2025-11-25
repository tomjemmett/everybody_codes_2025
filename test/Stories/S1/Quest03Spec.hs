module Stories.S1.Quest03Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Story 1, Quest 2" $ do
  it "Sample" $ do
    s <- s1q3 "sample"
    s `shouldBe` (1310, 13659, 13659)

  it "Actual" $ do
    a <- s1q3 "actual"
    a `shouldBe` (3444, 1029790, 90312279547)
