{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Scripts.DoTheThingSpec (spec) where

import qualified Data.Text as T
import Numeric.Natural
import Scripts.DoTheThing
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "parseResourceIdentifier" $ do
    it "matches simple filenames" $
      parseResourceIdentifier "README.md" `shouldBe` File (FilePathNoAddress "README.md")
    it "matches filenames with trailing colon" $
      parseResourceIdentifier "README.md:" `shouldBe` File (FilePathNoAddress "README.md")
    it "matches filenames with line" $
      parseResourceIdentifier "README.md:4" `shouldBe` File (FilePathLineAddress "README.md" 4)
    it "matches filenames with line and trailing collon" $
      parseResourceIdentifier "README.md:3:" `shouldBe` File (FilePathLineAddress "README.md" 3)
    it "matches filenames with line and trailing word" $
      parseResourceIdentifier "README.md:3:match" `shouldBe` File (FilePathLineAddress "README.md" 3)
    it "matches filenames with line and column" $
      parseResourceIdentifier "README.md:3:4" `shouldBe` File (FilePathLineColumnAddress "README.md" 3 4)
    it "matches filenames fith line, column and trailing colon" $
      parseResourceIdentifier "README.md:3:4:" `shouldBe` File (FilePathLineColumnAddress "README.md" 3 4)
    it "matches hlint multi line output" $
      parseResourceIdentifier "README.md:(30,5)-(31,62):" `shouldBe` File (FilePathRangeAddress "README.md" 30 5 31 62)
    it "matches hlint single line output" $
      parseResourceIdentifier "README.md:42:117-119:" `shouldBe` File (FilePathRangeAddress "README.md" 42 117 42 119)

    it "matches man pages" $
      parseResourceIdentifier "ls(1)" `shouldBe` ManPage "ls" 1

    it "matches git short commit hashes" $
      parseResourceIdentifier "7437dd8" `shouldBe` GitCommit "7437dd8"
    it "matches git long commit hashes" $
      parseResourceIdentifier "7437dd88ba8ffb7648ab1bb32fe1465851f2804f" `shouldBe` GitCommit "7437dd88ba8ffb7648ab1bb32fe1465851f2804f"
    it "not matches filenames with timestamps as git hashes" $
      parseResourceIdentifier "20210113T005730.txt" `shouldBe` File (FilePathNoAddress "20210113T005730.txt")

    it "matches http URL" $
      parseResourceIdentifier "http://www.haskell.org/" `shouldBe` URL "http://www.haskell.org/"
    it "matches https URL" $
      parseResourceIdentifier "https://www.haskell.org/" `shouldBe` URL "https://www.haskell.org/"
    it "matches file URL" $
      parseResourceIdentifier "file:///foo.bar" `shouldBe` URL "file:///foo.bar"

  describe "doTheThingForResourceIdentifier" $ do
    describe "unkown editor" $ do
      it "handles FilePathNoAddress" $
        property $ \config NonEmptyText {fromNonEmptyText = editor} ->
          doTheThingForResourceIdentifier (config {configEditor = Unknown editor}) (File $ FilePathNoAddress "README.md")
            `shouldBe` editor <> " README.md"
      it "handles FilePathLineAddress" $
        property $ \config NonEmptyText {fromNonEmptyText = editor} ->
          doTheThingForResourceIdentifier (config {configEditor = Unknown editor}) (File $ FilePathLineAddress "README.md" 5)
            `shouldBe` editor <> " README.md"
      it "handles FilePathLineColumnAddress" $
        property $ \config NonEmptyText {fromNonEmptyText = editor} ->
          doTheThingForResourceIdentifier (config {configEditor = Unknown editor}) (File $ FilePathLineColumnAddress "README.md" 5 7)
            `shouldBe` editor <> " README.md"

  describe "doTheThingForResourceIdentifier" $ do
    describe "vis" $ do
      it "handles FilePathNoAddress" $
        property $ \config ->
          doTheThingForResourceIdentifier (config {configEditor = Vis}) (File $ FilePathNoAddress "README.md")
            `shouldBe` "vis README.md"
      it "handles FilePathLineAddress" $
        property $ \config ->
          doTheThingForResourceIdentifier (config {configEditor = Vis}) (File $ FilePathLineAddress "README.md" 5)
            `shouldBe` "vis +5-#0 README.md"
      it "handles FilePathLineColumnAddress" $
        property $ \config ->
          doTheThingForResourceIdentifier (config {configEditor = Vis}) (File $ FilePathLineColumnAddress "README.md" 5 7)
            `shouldBe` "vis +5-#0+#7-#1 README.md"
      it "handles FilePathRangeAddress" $
        property $ \config ->
          doTheThingForResourceIdentifier (config {configEditor = Vis}) (File $ FilePathRangeAddress "README.md" 5 7 6 10)
            `shouldBe` "vis +5-#0+#7-#1,6-#0+#10 README.md"

    describe "plumb" $ do
      it "handles FilePathNoAddress" $
        property $ \config ->
          doTheThingForResourceIdentifier (config {configEditor = Plumb}) (File $ FilePathNoAddress "README.md")
            `shouldBe` "plumb -d edit $(pwd)/README.md"
      it "handles FilePathLineAddress" $
        property $ \config ->
          doTheThingForResourceIdentifier (config {configEditor = Plumb}) (File $ FilePathLineAddress "README.md" 5)
            `shouldBe` "plumb -d edit -a 'addr=5' $(pwd)/README.md"
      it "handles FilePathLineColumnAddress" $
        property $ \config ->
          doTheThingForResourceIdentifier (config {configEditor = Plumb}) (File $ FilePathLineColumnAddress "README.md" 5 7)
            `shouldBe` "plumb -d edit -a 'addr=5-#0+#7-#1' $(pwd)/README.md"
      it "handles FilePathRangeAddress" $
        property $ \config ->
          doTheThingForResourceIdentifier (config {configEditor = Plumb}) (File $ FilePathRangeAddress "README.md" 5 7 6 10)
            `shouldBe` "plumb -d edit -a 'addr=5-#0+#7-#1,6-#0+#10' $(pwd)/README.md"

    describe "9" $ do
      it "handles FilePathNoAddress" $
        property $ \config ->
          doTheThingForResourceIdentifier (config {configEditor = Nine Plumb}) (File $ FilePathNoAddress "README.md")
            `shouldBe` "9 plumb -d edit $(pwd)/README.md"
      it "handles FilePathLineAddress" $
        property $ \config ->
          doTheThingForResourceIdentifier (config {configEditor = Nine Plumb}) (File $ FilePathLineAddress "README.md" 5)
            `shouldBe` "9 plumb -d edit -a 'addr=5' $(pwd)/README.md"
      it "handles FilePathLineColumnAddress" $
        property $ \config ->
          doTheThingForResourceIdentifier (config {configEditor = Nine Plumb}) (File $ FilePathLineColumnAddress "README.md" 5 7)
            `shouldBe` "9 plumb -d edit -a 'addr=5-#0+#7-#1' $(pwd)/README.md"
      it "handles FilePathRangeAddress" $
        property $ \config ->
          doTheThingForResourceIdentifier (config {configEditor = Nine Plumb}) (File $ FilePathRangeAddress "README.md" 5 7 6 10)
            `shouldBe` "9 plumb -d edit -a 'addr=5-#0+#7-#1,6-#0+#10' $(pwd)/README.md"

    describe "emacsclient" $ do
      it "handles FilePathNoAddress" $
        property $ \config ->
          doTheThingForResourceIdentifier (config {configEditor = EmacsClient}) (File $ FilePathNoAddress "README.md")
            `shouldBe` "emacsclient -n -a '' README.md"
      it "handles FilePathLineAddress" $
        property $ \config ->
          doTheThingForResourceIdentifier (config {configEditor = EmacsClient}) (File $ FilePathLineAddress "README.md" 5)
            `shouldBe` "emacsclient -n -a '' +5 README.md"
      it "handles FilePathLineColumnAddress" $
        property $ \config ->
          doTheThingForResourceIdentifier (config {configEditor = EmacsClient}) (File $ FilePathLineColumnAddress "README.md" 5 7)
            `shouldBe` "emacsclient -n -a '' +5:7 README.md"
      it "handles FilePathRangeAddress" $
        property $ \config ->
          doTheThingForResourceIdentifier (config {configEditor = EmacsClient}) (File $ FilePathRangeAddress "README.md" 5 7 6 10)
            `shouldBe` "emacsclient -n -a '' +5:7 README.md"

    describe "vscodium" $ do
      it "handles FilePathNoAddress" $
        property $ \config ->
          doTheThingForResourceIdentifier (config {configEditor = VSCodium}) (File $ FilePathNoAddress "README.md")
            `shouldBe` "codium -g README.md"
      it "handles FilePathLineAddress" $
        property $ \config ->
          doTheThingForResourceIdentifier (config {configEditor = VSCodium}) (File $ FilePathLineAddress "README.md" 5)
            `shouldBe` "codium -g README.md:5"
      it "handles FilePathLineColumnAddress" $
        property $ \config ->
          doTheThingForResourceIdentifier (config {configEditor = VSCodium}) (File $ FilePathLineColumnAddress "README.md" 5 7)
            `shouldBe` "codium -g README.md:5:7"
      it "handles FilePathRangeAddress" $
        property $ \config ->
          doTheThingForResourceIdentifier (config {configEditor = VSCodium}) (File $ FilePathRangeAddress "README.md" 5 7 6 10)
            `shouldBe` "codium -g README.md:5:7"

    it "handles ManPage" $
      property $ \config ->
        doTheThingForResourceIdentifier config (ManPage "foo" 7)
          `shouldBe` "man 7 foo"
    it "handles GitCommit" $
      property $ \config ->
        doTheThingForResourceIdentifier config (GitCommit "7437dd88ba8ffb7648ab1bb32fe1465851f2804f")
          `shouldBe` "git show 7437dd88ba8ffb7648ab1bb32fe1465851f2804f"
    it "handles URL" $
      property $ \config ->
        doTheThingForResourceIdentifier config (URL "https://www.haskell.org/")
          `shouldBe` "chromium https://www.haskell.org/"

  describe "parse . inspect" $ do
    it "is the identity" $
      property $
        \r -> parseResourceIdentifier (inspect r) `shouldBe` r

  describe "doTheThingCommand" $ do
    it "behaves like doTheThingForResourceIdentifier on parseResourceIdentifier" $
      property $
        \config r -> doTheThingCommand config (inspect r) `shouldBe` doTheThingForResourceIdentifier config (parseResourceIdentifier (inspect r))

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
arbNatural = fromInteger . abs <$> arbitrary

instance Arbitrary ResourceIdentifier where
  arbitrary =
    oneof
      [ File <$> arbitrary,
        ManPage <$> arbNonEmptyText <*> arbNatural
      ]

instance Arbitrary FilePathAddress where
  arbitrary =
    oneof
      [ FilePathNoAddress <$> arbFilePath,
        FilePathLineAddress <$> arbFilePath <*> arbNatural,
        FilePathLineColumnAddress <$> arbFilePath <*> arbNatural <*> arbNatural,
        FilePathRangeAddress <$> arbFilePath <*> arbNatural <*> arbNatural <*> arbNatural <*> arbNatural
      ]

instance Arbitrary Config where
  arbitrary = Config <$> arbitrary

instance Arbitrary Editor where
  arbitrary =
    oneof
      [ return Vis,
        return Plumb,
        Unknown <$> arbNonEmptyText
      ]

newtype NonEmptyText = NonEmptyText {fromNonEmptyText :: T.Text}
  deriving (Show, Eq)

instance Arbitrary NonEmptyText where
  arbitrary = NonEmptyText <$> arbNonEmptyText
