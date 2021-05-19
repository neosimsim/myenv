{-# OPTIONS_GHC -Wall -Werror #-}

import Control.Monad
import Data.Functor
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Environment
import System.Exit

endsWithNewLine :: FilePath -> IO Bool
endsWithNewLine file = T.readFile file <&> T.last >.> (/= '\n')

main :: IO ()
main = do
  files <- getArgs
  filesWithoutN <- filterM endsWithNewLine files
  unless (null filesWithoutN) $ do
    unless (length files == 1) $ mapM_ putStrLn files
    exitFailure

(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)
