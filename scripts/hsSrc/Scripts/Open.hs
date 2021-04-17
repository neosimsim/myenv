{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Scripts.Open
  ( main,
    ResourceIdentifier (..),
    parseResourceIdentifier,
    openResourceIdentifierCommand,
    inspect,
    openCommand,
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
import System.Environment (getArgs)
import Text.RawString.QQ
import Text.Regex.TDFA

data ResourceIdentifier
  = FilePathNoAddress Text
  | FilePathLineAddress Text Natural
  | FilePathLineColumnAddress Text Natural Natural
  | ManPage Text Natural
  | GitCommit Text
  | URL Text
  deriving (Show, Eq)

parseResourceIdentifier :: Text -> ResourceIdentifier
parseResourceIdentifier x =
  fromMaybe (parseFilePathAddress x) $
    parseManPage x
      <|> parseURL x
      <|> parseGitCommit x

parseFilePathAddress :: Text -> ResourceIdentifier
parseFilePathAddress x = case (x =~ filePathExpr :: (Text, Text, Text, [Text])) of
  (_, _, _, [filePath, _, line, _, ""]) ->
    fallback $
      FilePathLineAddress filePath <$> readMay (T.unpack line)
  (_, _, _, [filePath, _, line, _, culomn]) ->
    fallback $
      FilePathLineColumnAddress filePath
        <$> readMay (T.unpack line)
        <*> readMay (T.unpack culomn)
  _ -> FilePathNoAddress x
  where
    filePathExpr :: Text
    filePathExpr = [r|([^:]+)(:([0-9]*)(:([0-9]*))?)?:?|]
    fallback = fromMaybe $ FilePathNoAddress x

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

openResourceIdentifierCommand :: ResourceIdentifier -> Text
openResourceIdentifierCommand (FilePathNoAddress path) =
  T.pack [i|vis #{path}|]
openResourceIdentifierCommand (FilePathLineAddress path line) =
  T.pack [i|vis +#{line}-\#0 #{path}|]
openResourceIdentifierCommand (FilePathLineColumnAddress path line 0) =
  T.pack [i|vis +#{line}-\#0 #{path}|]
openResourceIdentifierCommand (FilePathLineColumnAddress path line column) =
  T.pack [i|vis +#{line}-\#0+\##{column - 1} #{path}|]
openResourceIdentifierCommand (ManPage cmd section) =
  T.pack [i|man #{section} #{cmd}|]
openResourceIdentifierCommand (GitCommit commit) =
  T.pack [i|git show #{commit}|]
openResourceIdentifierCommand (URL url) =
  T.pack [i|chromium #{url}|]

inspect :: ResourceIdentifier -> Text
inspect (FilePathNoAddress path) = path
inspect (FilePathLineAddress path line) = T.pack [i|#{path}:#{line}|]
inspect (FilePathLineColumnAddress path line column) = T.pack [i|#{path}:#{line}:#{column}|]
inspect (ManPage cmd section) = T.pack [i|#{cmd}(#{section})|]
inspect (GitCommit commit) = commit
inspect (URL url) = url

openCommand :: Text -> Text
openCommand = openResourceIdentifierCommand . parseResourceIdentifier

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> T.putStrLn . openCommand $ T.pack arg
    _ -> return ()
