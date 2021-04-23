{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Scripts.Open
  ( main,
    ResourceIdentifier (..),
    parseResourceIdentifier,
    openResourceIdentifierCommand,
    inspect,
    openCommand,
    Editor (..),
    FilePathAddress (..),
    Config (..),
  )
where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Numeric.Natural (Natural)
import Safe
import System.Environment (getArgs, lookupEnv)
import Text.RawString.QQ
import Text.Regex.TDFA

newtype Config = Config {configEditor :: Editor}
  deriving (Show, Eq)

data Editor
  = Plumb
  | Vis
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
  deriving (Show, Eq)

parseResourceIdentifier :: Text -> ResourceIdentifier
parseResourceIdentifier x =
  fromMaybe (File $ FilePathNoAddress x) $
    parseManPage x
      <|> parseURL x
      <|> parseGitCommit x
      <|> File <$> parseFilePathAddress x

parseFilePathAddress :: Text -> Maybe FilePathAddress
parseFilePathAddress
  x = case (x =~ filePathExpr :: (Text, Text, Text, [Text])) of
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
    gitCommitExpr = [r|[a-f0-9]{7}|]

openResourceIdentifierCommand :: Config -> ResourceIdentifier -> Text
openResourceIdentifierCommand _ (ManPage cmd section) =
  T.pack [i|man #{section} #{cmd}|]
openResourceIdentifierCommand _ (GitCommit commit) =
  T.pack [i|git show #{commit}|]
openResourceIdentifierCommand _ (URL url) =
  T.pack [i|chromium #{url}|]
openResourceIdentifierCommand Config {configEditor = Vis} (File pathAddress) = openResourceIdentifierCommandWithVis pathAddress
openResourceIdentifierCommand Config {configEditor = Plumb} (File pathAddress) = openResourceIdentifierCommandWithPlumb pathAddress
openResourceIdentifierCommand Config {configEditor = Unknown editorName} (File pathAddress) = openResourceIdentifierCommandWithEditor editorName pathAddress

openResourceIdentifierCommandWithEditor :: Text -> FilePathAddress -> Text
openResourceIdentifierCommandWithEditor cmd (FilePathNoAddress path) =
  T.pack [i|#{cmd} #{path}|]
openResourceIdentifierCommandWithEditor cmd (FilePathLineAddress path _) =
  T.pack [i|#{cmd} #{path}|]
openResourceIdentifierCommandWithEditor cmd (FilePathLineColumnAddress path _ _) =
  T.pack [i|#{cmd} #{path}|]

openResourceIdentifierCommandWithVis :: FilePathAddress -> Text
openResourceIdentifierCommandWithVis (FilePathNoAddress path) =
  T.pack [i|vis #{path}|]
openResourceIdentifierCommandWithVis (FilePathLineAddress path line) =
  T.pack [i|vis +#{line}-\#0 #{path}|]
openResourceIdentifierCommandWithVis (FilePathLineColumnAddress path line 0) =
  T.pack [i|vis +#{line}-\#0 #{path}|]
openResourceIdentifierCommandWithVis (FilePathLineColumnAddress path line column) =
  T.pack [i|vis +#{line}-\#0+\##{column}-#1 #{path}|]

openResourceIdentifierCommandWithPlumb :: FilePathAddress -> Text
openResourceIdentifierCommandWithPlumb (FilePathNoAddress path) =
  T.pack [i|plumb -d edit `{pwd}^/#{path}|]
openResourceIdentifierCommandWithPlumb (FilePathLineAddress path line) =
  T.pack [i|plumb -d edit -a 'addr=#{line}' `{pwd}^/#{path}|]
openResourceIdentifierCommandWithPlumb (FilePathLineColumnAddress path line 0) =
  T.pack [i|plumb -d edit -a 'addr=#{line}:0' `{pwd}^/#{path}|]
openResourceIdentifierCommandWithPlumb (FilePathLineColumnAddress path line column) =
  T.pack [i|plumb -d edit -a 'addr=#{line}-\#0+\##{column}-\#1' `{pwd}^/#{path}|]

inspect :: ResourceIdentifier -> Text
inspect (File (FilePathNoAddress path)) = path
inspect (File (FilePathLineAddress path line)) = T.pack [i|#{path}:#{line}|]
inspect (File (FilePathLineColumnAddress path line column)) = T.pack [i|#{path}:#{line}:#{column}|]
inspect (ManPage cmd section) = T.pack [i|#{cmd}(#{section})|]
inspect (GitCommit commit) = commit
inspect (URL url) = url

openCommand :: Config -> Text -> Text
openCommand c = openResourceIdentifierCommand c . parseResourceIdentifier

getConfig :: IO Config
getConfig = Config <$> getEditor

getEditor :: IO Editor
getEditor = do
  editor <- fromMaybe "vis" <$> lookupEnv "EDITOR"
  case editor of
    "vis" -> return Vis
    "editinacme" -> return Plumb
    "B" -> return Plumb
    "E" -> return Plumb
    x -> return $ Unknown $ T.pack x

main :: IO ()
main = do
  args <- getArgs
  config <- getConfig
  case args of
    [arg] -> T.putStrLn . openCommand config $ T.pack arg
    _ -> return ()
