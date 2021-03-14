{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Scripts.Open
  ( main,
    FilePathAddress (..),
    parseFilePathAddress,
  )
where

import Data.Maybe (fromMaybe)
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Numeric.Natural (Natural)
import Safe
import System.Environment (getArgs)
import Text.Regex.TDFA

data FilePathAddress
  = FilePathNoAddress Text
  | FilePathLineAddress Text Natural
  | FilePathLineColumnAddress Text Natural Natural
  deriving (Show, Eq)

parseFilePathAddress :: Text -> FilePathAddress
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
    filePathExpr = "([^:]+)(:([0-9]*)(:([0-9]*))?)?:?"
    fallback = fromMaybe $ FilePathNoAddress x

openFilePathAddressCommand :: FilePathAddress -> Text
openFilePathAddressCommand (FilePathNoAddress path) =
  T.pack [i|vis #{path}|]
openFilePathAddressCommand (FilePathLineAddress path line) =
  T.pack [i|vis +#{line}-\#0 #{path}|]
openFilePathAddressCommand (FilePathLineColumnAddress path line column) =
  T.pack [i|vis +#{line}-\#0+\##{column}-\#1 #{path}|]

openCommand :: Text -> Text
openCommand = openFilePathAddressCommand . parseFilePathAddress

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> T.putStrLn . openCommand $ T.pack arg
    _ -> return ()
