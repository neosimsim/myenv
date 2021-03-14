{-# LANGUAGE OverloadedStrings #-}

module Scripts.OpenSpec (spec) where

import Scripts.Open
import Test.Hspec

spec :: Spec
spec =
  describe "Open.filePathAddress" $ do
    it "matches simple filenames" $
      parseFilePathAddress "README.md" `shouldBe` Just (FilePathNoAddress "README.md")
    it "matches filenames with line" $
      parseFilePathAddress "README.md:4" `shouldBe` Just (FilePathLineAddress "README.md" 4)
    it "matches filenames with line and missing column" $
      parseFilePathAddress "README.md:3:" `shouldBe` Just (FilePathLineAddress "README.md" 3)
    it "matches filenames with line and column" $
      parseFilePathAddress "README.md:3:4" `shouldBe` Just (FilePathLineColumnAddress "README.md" 3 4)
