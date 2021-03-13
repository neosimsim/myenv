{-# LANGUAGE OverloadedStrings #-}

module Scripts.OpenSpec (spec) where

import Open
import Test.Hspec

spec :: Spec
spec =
  describe "Open.filePathAddress" $ do
    it "matches simple filenames" $
      filePathAddress "README.md" `shouldBe` Just (FilePathNoAddress "README.md")
    it "matches filenames with line" $
      filePathAddress "README.md:3" `shouldBe` Just (FilePathLineAddress "README.md" 3)
    it "matches filenames with line and missing column" $
      filePathAddress "README.md:3:" `shouldBe` Just (FilePathLineAddress "README.md" 3)
    it "matches filenames with line and column" $
      filePathAddress "README.md:3:4" `shouldBe` Just (FilePathLineColumnAddress "README.md" 3 4)
