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
  describe "parseResourceIdentifier" $ do
    it "matches simple filenames" $
      parseResourceIdentifier "README.md" `shouldBe` FilePathNoAddress "README.md"
    it "matches filenames with line" $
      parseResourceIdentifier "README.md:4" `shouldBe` FilePathLineAddress "README.md" 4
    it "matches filenames with line and missing column" $
      parseResourceIdentifier "README.md:3:" `shouldBe` FilePathLineAddress "README.md" 3
    it "matches filenames with line and GNU grep match" $
      parseResourceIdentifier "README.md:3:match" `shouldBe` FilePathLineAddress "README.md" 3
    it "matches filenames with line and column" $
      parseResourceIdentifier "README.md:3:4" `shouldBe` FilePathLineColumnAddress "README.md" 3 4
    it "matches filenames with line and column with trailing colon (GHC)" $
      parseResourceIdentifier "README.md:3:4:" `shouldBe` FilePathLineColumnAddress "README.md" 3 4
    it "matches hlint output" $ pendingWith "not implemented"
    -- parseResourceIdentifier "hs/Scripts/Open.hs:(30,5)-(31,62):" `shouldBe` _
    -- parseResourceIdentifier "hsTest/Scripts/OpenSpec.hs:42:117-119:" `shouldBe` _

    it "matches man pages" $
      parseResourceIdentifier "ls(1)" `shouldBe` ManPage "ls" 1

    it "matches git commits pages" $
      parseResourceIdentifier "7437dd88ba8ffb7648ab1bb32fe1465851f2804f" `shouldBe` GitCommit "7437dd88ba8ffb7648ab1bb32fe1465851f2804f"

    it "matches http URL" $
      parseResourceIdentifier "http://www.haskell.org/" `shouldBe` URL "http://www.haskell.org/"
    it "matches https URL" $
      parseResourceIdentifier "https://www.haskell.org/" `shouldBe` URL "https://www.haskell.org/"
    it "matches file URL" $
      parseResourceIdentifier "file:///foo.bar" `shouldBe` URL "file:///foo.bar"

  describe "openResourceIdentifierCommand" $ do
    it "handles FilePathNoAddress" $
      openResourceIdentifierCommand (FilePathNoAddress "README.md") `shouldBe` "vis README.md"
    it "handles FilePathLineAddress" $
      openResourceIdentifierCommand (FilePathLineAddress "README.md" 5) `shouldBe` "vis +5-#0 README.md"
    it "handles FilePathLineColumnAddress" $
      openResourceIdentifierCommand (FilePathLineColumnAddress "README.md" 5 7) `shouldBe` "vis +5-#0+#6 README.md"
    it "handles ManPage" $
      openResourceIdentifierCommand (ManPage "foo" 7) `shouldBe` "man 7 foo"
    it "handles GitCommit" $
      openResourceIdentifierCommand (GitCommit "7437dd88ba8ffb7648ab1bb32fe1465851f2804f") `shouldBe` "git show 7437dd88ba8ffb7648ab1bb32fe1465851f2804f"
    it "handles URL" $
      openResourceIdentifierCommand (URL "https://www.haskell.org/") `shouldBe` "firefox https://www.haskell.org/"

  describe "parse . inspect" $ do
    it "is the identity" $
      property $ \r -> parseResourceIdentifier (inspect r) `shouldBe` r

  describe "openCommand" $ do
    it "behaves like openResourceIdentifierCommand on parseResourceIdentifier" $
      property $ \r -> openCommand (inspect r) `shouldBe` openResourceIdentifierCommand (parseResourceIdentifier (inspect r))

arbNonEmptyText :: Gen T.Text
arbNonEmptyText =
  T.pack
    <$> ( (:)
            <$> arbitraryPrintableChar
            <*> (getPrintableString <$> arbitrary)
        )

-- | Non-empty printable string, not containing ̈':'
arbFilePath :: Gen T.Text
arbFilePath = do
  path <- T.filter (/= ':') <$> arbNonEmptyText
  if T.null path
    then arbFilePath
    else return path

arbNatural :: Gen Natural
arbNatural = fromInteger <$> chooseInteger (0, 100)

instance Arbitrary ResourceIdentifier where
  arbitrary =
    oneof
      [ FilePathNoAddress <$> arbFilePath,
        FilePathLineAddress <$> arbFilePath <*> arbNatural,
        FilePathLineColumnAddress <$> arbFilePath <*> arbNatural <*> arbNatural,
        ManPage <$> arbNonEmptyText <*> arbNatural
      ]
