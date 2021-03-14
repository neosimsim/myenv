{-# LANGUAGE OverloadedStrings #-}

module Scripts.OpenSpec (spec) where

import Scripts.Open
import Test.Hspec

spec :: Spec
spec =
  describe "Open.filePathAddress" $ do
    it "matches simple filenames" $
      parseFilePathAddress "README.md" `shouldBe` FilePathNoAddress "README.md"
    it "matches filenames with line" $
      parseFilePathAddress "README.md:4" `shouldBe` FilePathLineAddress "README.md" 4
    it "matches filenames with line and missing column" $
      parseFilePathAddress "README.md:3:" `shouldBe` FilePathLineAddress "README.md" 3
    it "matches filenames with line and GNU grep match" $
      parseFilePathAddress "README.md:3:match" `shouldBe` FilePathLineAddress "README.md" 3
    it "matches filenames with line and column" $
      parseFilePathAddress "README.md:3:4" `shouldBe` FilePathLineColumnAddress "README.md" 3 4
    it "matches filenames with line and column with trailing colon (GHC)" $
      parseFilePathAddress "README.md:3:4:" `shouldBe` FilePathLineColumnAddress "README.md" 3 4
    it "matches hlint output" $ pendingWith "not implemented" -- hs/Scripts/Open.hs:(30,5)-(31,62):
