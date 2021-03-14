{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Scripts.Open
  ( main,
    FilePathAddress (..),
    parseFilePathAddress,
  )
where

import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Numeric.Natural (Natural)
import Safe
import System.Environment (getArgs)
import Text.Regex.TDFA

data FilePathAddress
  = FilePathNoAddress Text
  | FilePathLineAddress Text Natural
  | FilePathLineColumnAddress Text Natural Natural
  deriving (Show, Eq)

parseFilePathAddress :: Text -> Maybe FilePathAddress
parseFilePathAddress x = case (x =~ filePathExpr :: (Text, Text, Text, [Text])) of
  (_, _, _, [filePath, _, "", _, ""]) ->
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
    filePathExpr = "^([^:]+)(:([0-9]+)(:([0-9]*))?)?$"

openFilePathAddressCommand :: FilePathAddress -> Text
openFilePathAddressCommand (FilePathNoAddress path) =
  T.pack [i|vis #{path}|]
openFilePathAddressCommand (FilePathLineAddress path line) =
  T.pack [i|vis +#{line}-\#0 #{path}|]
openFilePathAddressCommand (FilePathLineColumnAddress path line column) =
  T.pack [i|vis +#{line}-\#0+\##{column}-\#1 #{path}|]

openCommand :: Text -> Maybe Text
openCommand x = openFilePathAddressCommand <$> parseFilePathAddress x

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> case openCommand (T.pack arg) of
      Nothing -> return ()
      Just cmd -> putStrLn $ T.unpack cmd
    _ -> return ()
