{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Scripts.OpenSpec (spec) where

import qualified Data.Text as T
import Numeric.Natural
import Scripts.Open
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "filePathAddress" $ do
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
    it "matches hlint output" $ pendingWith "not implemented"
  -- parseFilePathAddress "hs/Scripts/Open.hs:(30,5)-(31,62):" `shouldBe` _
  -- parseFilePathAddress "hsTest/Scripts/OpenSpec.hs:42:117-119:" `shouldBe` _

  describe "openFilePathAddressCommand" $ do
    it "handles FilePathNoAddress" $
      openFilePathAddressCommand (FilePathNoAddress "README.md") `shouldBe` "vis README.md"
    it "handles FilePathLineAddress" $
      openFilePathAddressCommand (FilePathLineAddress "README.md" 5) `shouldBe` "vis +5-#0 README.md"
    it "handles FilePathLineColumnAddress" $
      openFilePathAddressCommand (FilePathLineColumnAddress "README.md" 5 7) `shouldBe` "vis +5-#0+#6 README.md"

  describe "parse . inspect" $ do
    it "identity" $
      property $ \r -> parseFilePathAddress (inspect r) `shouldBe` r

  describe "openCommand" $ do
    it "behaves like openFilePathAddressCommand on parseFilePathAddress" $
      property $ \r -> openCommand (inspect r) `shouldBe` openFilePathAddressCommand (parseFilePathAddress (inspect r))

-- | Non-empty printable string, not containing ̈':'
arbFilePath :: Gen T.Text
arbFilePath = do
  path <-
    T.pack . filter (/= ':')
      <$> ( (:)
              <$> arbitraryPrintableChar
              <*> (getPrintableString <$> arbitrary)
          )
  if T.null path
    then arbFilePath
    else return path

arbNatural :: Gen Natural
arbNatural = fromInteger <$> chooseInteger (0, 100)

instance Arbitrary FilePathAddress where
  arbitrary =
    oneof
      [ FilePathNoAddress <$> arbFilePath,
        FilePathLineAddress <$> arbFilePath <*> arbNatural,
        FilePathLineColumnAddress <$> arbFilePath <*> arbNatural <*> arbNatural
      ]
