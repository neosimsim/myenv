{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main,
  )
where

import Control.Applicative (Alternative, liftA2, many)
import qualified Data.ByteString.Lazy.UTF8 as BL (toString)
import Data.Finite (Finite, packFinite)
import Data.List.NonEmpty as List (NonEmpty ((:|)))
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import Data.Vector.Sized as Vector (Vector)
import qualified Data.Vector.Sized as Vector (fromList, imapM, toList)
import GHC.TypeNats
  ( KnownNat,
    SomeNat (SomeNat),
    someNatVal,
  )
import Options.Applicative
  ( Parser,
    argument,
    auto,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    metavar,
    option,
    short,
    showDefault,
    str,
    value,
    (<**>),
  )
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Typed (proc, readProcess, setStdin)

data Options = Options
  { column :: Int,
    shellCmd :: List.NonEmpty String
  }
  deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> option
      auto
      ( short 'c'
          <> help "Column to map, counting from 1. 0 means whole line"
          <> showDefault
          <> value 0
          <> metavar "COLUMN"
      )
    <*> some' (argument str (metavar "SHELL_CMD..."))

some' :: (Alternative f) => f a -> f (List.NonEmpty a)
some' v = liftA2 (:|) v (many v)

parseOptions :: IO Options
parseOptions =
  execParser
    ( info
        (optionsParser <**> helper)
        (fullDesc <> header "map - map lines of text")
    )

withSizedLists ::
  forall a r.
  [[a]] ->
  ( forall n.
    (KnownNat n) =>
    [Vector n a] ->
    r
  ) ->
  r
withSizedLists [] f = f @0 []
withSizedLists list@(x : _) f =
  let len = length x
   in case someNatVal (fromIntegral len) of
        SomeNat (Proxy :: Proxy l) ->
          let maybeVecs = (Vector.fromList <$> list :: [Maybe (Vector l a)])
           in case sequence maybeVecs of
                Just xs -> f xs
                Nothing ->
                  error $
                    "not all lines have the same length: " ++ show (length x)

mapAtM ::
  forall n m a.
  (Monad m) =>
  Finite n ->
  (a -> m a) ->
  Vector n a ->
  m (Vector n a)
mapAtM i f = Vector.imapM _innermap
  where
    _innermap j
      | i == j = f
      | otherwise = return

mapShell :: List.NonEmpty String -> String -> IO String
mapShell (x :| xs) input = do
  (exitCode, out, err) <- readProcess $ setStdin (fromString input) (proc x xs)
  case exitCode of
    ExitSuccess -> return $ BL.toString out
    ExitFailure _ -> error $ BL.toString err

main :: IO ()
main = do
  putStrLn "Hello"
  opts <- parseOptions
  print opts
  table <- getInputTable
  withSizedLists table $ \vecs ->
    case packFinite (fromIntegral (column opts - 1)) of
      Just (fIndex :: Finite n) -> do
        newVec <- mapM (mapAtM fIndex (mapShell (shellCmd opts))) vecs
        putStr . unlines $ fmap (unwords . Vector.toList) newVec
      Nothing -> error "index out of bounds"
  putStrLn "bye"
  where
    getInputTable :: IO [[String]]
    getInputTable = fmap words . lines <$> getContents
