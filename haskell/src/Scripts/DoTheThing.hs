{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Scripts.DoTheThing
  ( main,
    ResourceIdentifier (..),
    parseResourceIdentifier,
    doTheThingForResourceIdentifier,
    inspect,
    doTheThingCommand,
    Editor (..),
    FilePathAddress (..),
    Config (..),
  )
where

import Control.Applicative ((<|>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.String.Interpolate
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Numeric.Natural (Natural)
import Safe
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import Text.RawString.QQ
import Text.Regex.TDFA

newtype Config = Config {configEditor :: Editor}
  deriving (Show, Eq)

data Editor
  = Plumb
  | Vis
  | EmacsClient
  | VSCodium
  | Nine Editor
  | Unknown Text
  deriving (Show, Eq)

data ResourceIdentifier
  = File FilePathAddress
  | ManPage Text Natural
  | GitCommit Text
  | URL Text
  deriving (Show, Eq)

data FilePathAddress
  = FilePathNoAddress Text
  | FilePathLineAddress Text Natural
  | FilePathLineColumnAddress Text Natural Natural
  | FilePathRangeAddress Text Natural Natural Natural Natural
  deriving (Show, Eq)

parseResourceIdentifier :: Text -> ResourceIdentifier
parseResourceIdentifier x =
  fromMaybe (File $ FilePathNoAddress x) $
    parseManPage x
      <|> parseURL x
      <|> parseGitCommit x
      <|> File <$> parseFilePathMultiLineRangeAddress x
      <|> File <$> parseFilePathSingleLineRangeAddress x
      <|> File <$> parseFilePathAddress x

parseFilePathAddress :: Text -> Maybe FilePathAddress
parseFilePathAddress x = case (x =~ filePathExpr :: (Text, Text, Text, [Text])) of
  -- trailing colon
  ("", _, "", [filePath, ":", "", "", ""]) ->
    Just $ FilePathNoAddress filePath
  (_, _, _, [filePath, _, line, _, ""]) ->
    FilePathLineAddress filePath <$> readMay (T.unpack line)
  (_, _, _, [filePath, _, line, _, culomn]) ->
    FilePathLineColumnAddress filePath
      <$> readMay (T.unpack line)
      <*> readMay (T.unpack culomn)
  _ -> Nothing
  where
    filePathExpr :: Text
    filePathExpr = [r|([^:]+)(:([0-9]*)(:([0-9]*))?)?:?|]

parseFilePathSingleLineRangeAddress :: Text -> Maybe FilePathAddress
parseFilePathSingleLineRangeAddress x = case (x =~ filePathExpr :: (Text, Text, Text, [Text])) of
  (_, _, _, [filePath, line, culomn1, culomn2]) ->
    FilePathRangeAddress filePath
      <$> readMay (T.unpack line)
      <*> readMay (T.unpack culomn1)
      <*> readMay (T.unpack line)
      <*> readMay (T.unpack culomn2)
  _ -> Nothing
  where
    filePathExpr :: Text
    filePathExpr = [r|([^:]+):([0-9]+):([0-9]+)-([0-9]+):?|]

parseFilePathMultiLineRangeAddress :: Text -> Maybe FilePathAddress
parseFilePathMultiLineRangeAddress x = case (x =~ filePathExpr :: (Text, Text, Text, [Text])) of
  (_, _, _, [filePath, line1, culomn1, line2, culomn2]) ->
    FilePathRangeAddress filePath
      <$> readMay (T.unpack line1)
      <*> readMay (T.unpack culomn1)
      <*> readMay (T.unpack line2)
      <*> readMay (T.unpack culomn2)
  _ -> Nothing
  where
    filePathExpr :: Text
    filePathExpr = [r|([^:]+):\(([0-9]+),([0-9]+)\)-\(([0-9]+),([0-9]+)\):?|]

parseManPage :: Text -> Maybe ResourceIdentifier
parseManPage x = case (x =~ manExpr :: (Text, Text, Text, [Text])) of
  (_, _, _, [cmd, section]) -> ManPage cmd <$> readMay (T.unpack section)
  _ -> Nothing
  where
    manExpr :: Text
    manExpr = [r|^(.+)\(([0-9]+)\)$|]

parseURL :: Text -> Maybe ResourceIdentifier
parseURL x =
  if x =~ urlExpr
    then Just $ URL x
    else Nothing
  where
    urlExpr :: Text
    urlExpr = [r|(https?|file)://.+$|]

parseGitCommit :: Text -> Maybe ResourceIdentifier
parseGitCommit x =
  if x =~ gitCommitExpr
    then Just $ GitCommit x
    else Nothing
  where
    gitCommitExpr :: Text
    gitCommitExpr = [r|^[a-f0-9]{7}$|^[a-f0-9]{40}$|]

doTheThingForResourceIdentifier :: Config -> ResourceIdentifier -> Text
doTheThingForResourceIdentifier _ (ManPage cmd section) =
  T.pack [i|man #{section} #{cmd}|]
doTheThingForResourceIdentifier _ (GitCommit commit) =
  T.pack [i|git show #{commit}|]
doTheThingForResourceIdentifier _ (URL url) =
  T.pack [i|chromium #{url}|]
doTheThingForResourceIdentifier Config {configEditor = Vis} (File pathAddress) = doTheThingForResourceIdentifierWithVis pathAddress
doTheThingForResourceIdentifier Config {configEditor = Plumb} (File pathAddress) = doTheThingForResourceIdentifierWithPlumb pathAddress
doTheThingForResourceIdentifier Config {configEditor = Nine editor} (File pathAddress) = "9 " <> doTheThingForResourceIdentifier (Config {configEditor = editor}) (File pathAddress)
doTheThingForResourceIdentifier Config {configEditor = EmacsClient} (File pathAddress) = doTheThingForResourceIdentifierWithEmacsclient pathAddress
doTheThingForResourceIdentifier Config {configEditor = VSCodium} (File pathAddress) = doTheThingForResourceIdentifierWithVSCodium pathAddress
doTheThingForResourceIdentifier Config {configEditor = Unknown editorName} (File pathAddress) = doTheThingForResourceIdentifierWithEditor editorName pathAddress

doTheThingForResourceIdentifierWithEditor :: Text -> FilePathAddress -> Text
doTheThingForResourceIdentifierWithEditor cmd (FilePathNoAddress path) =
  T.pack [i|#{cmd} #{path}|]
doTheThingForResourceIdentifierWithEditor cmd (FilePathLineAddress path _) =
  T.pack [i|#{cmd} #{path}|]
doTheThingForResourceIdentifierWithEditor cmd (FilePathLineColumnAddress path _ _) =
  T.pack [i|#{cmd} #{path}|]
doTheThingForResourceIdentifierWithEditor cmd (FilePathRangeAddress path _ _ _ _) =
  T.pack [i|#{cmd} #{path}|]

doTheThingForResourceIdentifierWithVis :: FilePathAddress -> Text
doTheThingForResourceIdentifierWithVis (FilePathNoAddress path) =
  T.pack [i|vis #{path}|]
doTheThingForResourceIdentifierWithVis (FilePathLineAddress path line) =
  T.pack [i|vis +#{line}-\#0 #{path}|]
doTheThingForResourceIdentifierWithVis (FilePathLineColumnAddress path line 0) =
  T.pack [i|vis +#{line}-\#0 #{path}|]
doTheThingForResourceIdentifierWithVis (FilePathLineColumnAddress path line column) =
  T.pack [i|vis +#{line}-\#0+\##{column}-\#1 #{path}|]
doTheThingForResourceIdentifierWithVis (FilePathRangeAddress path line1 column1 line2 column2) =
  T.pack [i|vis +#{line1}-\#0+\##{column1}-\#1,#{line2}-\#0+\##{column2} #{path}|]

doTheThingForResourceIdentifierWithPlumb :: FilePathAddress -> Text
doTheThingForResourceIdentifierWithPlumb (FilePathNoAddress path) =
  T.pack [i|plumb -d edit $(pwd)/#{path}|]
doTheThingForResourceIdentifierWithPlumb (FilePathLineAddress path line) =
  T.pack [i|plumb -d edit -a 'addr=#{line}' $(pwd)/#{path}|]
doTheThingForResourceIdentifierWithPlumb (FilePathLineColumnAddress path line 0) =
  T.pack [i|plumb -d edit -a 'addr=#{line}:0' $(pwd)/#{path}|]
doTheThingForResourceIdentifierWithPlumb (FilePathLineColumnAddress path line column) =
  T.pack [i|plumb -d edit -a 'addr=#{line}-\#0+\##{column}-\#1' $(pwd)/#{path}|]
doTheThingForResourceIdentifierWithPlumb (FilePathRangeAddress path line1 column1 line2 column2) =
  T.pack [i|plumb -d edit -a 'addr=#{line1}-\#0+\##{column1}-\#1,#{line2}-\#0+\##{column2}' $(pwd)/#{path}|]

doTheThingForResourceIdentifierWithEmacsclient :: FilePathAddress -> Text
doTheThingForResourceIdentifierWithEmacsclient (FilePathNoAddress path) =
  T.pack [i|emacsclient -n -a '' #{path}|]
doTheThingForResourceIdentifierWithEmacsclient (FilePathLineAddress path line) =
  T.pack [i|emacsclient -n -a '' +#{line} #{path}|]
doTheThingForResourceIdentifierWithEmacsclient (FilePathLineColumnAddress path line column) =
  T.pack [i|emacsclient -n -a '' +#{line}:#{column} #{path}|]
doTheThingForResourceIdentifierWithEmacsclient (FilePathRangeAddress path line1 column1 _line2 _column2) =
  T.pack [i|emacsclient -n -a '' +#{line1}:#{column1} #{path}|]

doTheThingForResourceIdentifierWithVSCodium :: FilePathAddress -> Text
doTheThingForResourceIdentifierWithVSCodium (FilePathNoAddress path) =
  T.pack [i|codium -g #{path}|]
doTheThingForResourceIdentifierWithVSCodium (FilePathLineAddress path line) =
  T.pack [i|codium -g #{path}:#{line}|]
doTheThingForResourceIdentifierWithVSCodium (FilePathLineColumnAddress path line column) =
  T.pack [i|codium -g #{path}:#{line}:#{column}|]
doTheThingForResourceIdentifierWithVSCodium (FilePathRangeAddress path line1 column1 _line2 _column2) =
  T.pack [i|codium -g #{path}:#{line1}:#{column1}|]

inspect :: ResourceIdentifier -> Text
inspect (File (FilePathNoAddress path)) = path
inspect (File (FilePathLineAddress path line)) = T.pack [i|#{path}:#{line}|]
inspect (File (FilePathLineColumnAddress path line column)) = T.pack [i|#{path}:#{line}:#{column}|]
inspect (File (FilePathRangeAddress path line1 column1 line2 column2)) = T.pack [i|#{path}:(#{line1},#{column1})-(#{line2},#{column2})|]
inspect (ManPage cmd section) = T.pack [i|#{cmd}(#{section})|]
inspect (GitCommit commit) = commit
inspect (URL url) = url

doTheThingCommand :: Config -> Text -> Text
doTheThingCommand c = doTheThingForResourceIdentifier c . parseResourceIdentifier

getConfig :: IO Config
getConfig = do
  editorCmd <- fromMaybe "vis" <$> lookupEnv "EDITOR"
  case NonEmpty.nonEmpty $ words editorCmd of
    Nothing -> do
      putStrLn "EDITOR is empty"
      exitFailure
    Just cmd -> case getEditor cmd of
      Left err -> do
        putStrLn err
        exitFailure
      Right editor -> return $ Config editor

getEditor :: NonEmpty.NonEmpty String -> Either String Editor
getEditor command = do
  -- Only consider the first word of EDITOR to ignore editor arguments.
  case command of
    "vis" :| _ -> return Vis
    "editinacme" :| _ -> return Plumb
    "sam" :| _ -> return Plumb
    "B" :| _ -> return Plumb
    "E" :| _ -> return Plumb
    "emacsclient" :| _ -> return EmacsClient
    "codium" :| _ -> return VSCodium
    "9" :| args -> case NonEmpty.nonEmpty args of
      Nothing -> Left "9 needs an editor command"
      Just moreArgs -> Nine <$> getEditor moreArgs
    x :| xs -> return $ Unknown $ T.pack $ mconcat $ x : xs

main :: IO ()
main = do
  args <- getArgs
  config <- getConfig
  case args of
    [arg] -> T.putStrLn . doTheThingCommand config $ T.pack arg
    _ -> return ()
