module Stories.S2.Quest02Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Story 2, Quest 2" $ do
  it "Sample" $ do
    s <- s2q2 "sample"
    s `shouldBe` (7, 2955, 2953681)

  it "Actual" $ do
    a <- s2q2 "actual"
    a `shouldBe` (131, 21497, 21199950)
