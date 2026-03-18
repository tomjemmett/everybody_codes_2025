module Stories.S3.Quest01Spec (spec) where

import SpecHelper
import Stories.S3.Quests

spec :: Spec
spec = describe "Story 3, Quest 1" $ do
  it "Sample" $ do
    s <- quest01 "sample"
    s `shouldBe` (9166, 2456, 292320)

  it "Actual" $ do
    a <- quest01 "actual"
    a `shouldBe` (48573, 82855, 11473775)
