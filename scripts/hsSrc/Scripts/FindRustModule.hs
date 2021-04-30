module Scripts.FindRustModule (moduleFiles, main) where

import Data.Sequences (splitSeq)
import System.Environment (getArgs)
import System.FilePath as FilePath
import System.FilePath.Glob (glob)
import System.Exit (exitFailure)

moduleFiles :: String -> [FilePath]
moduleFiles = moduleFiles_ . splitSeq "::"

moduleFiles_ :: [FilePath] -> [FilePath]
moduleFiles_ [] = []
moduleFiles_ [name] =
  [ name <.> "rs",
    name </> "mod" <.> "rs",
    "*" </> name <.> "rs",
    "*" </> name </> "mod" <.> "rs"
  ]
moduleFiles_ ("self" : segments) = moduleFiles_ segments
moduleFiles_ ("super" : segments) =
  [ joinPath segments <.> "rs",
    joinPath segments </> "mod" <.> "rs",
    ".." </> joinPath segments <.> "rs",
    ".." </> joinPath segments </> "mod" <.> "rs"
  ]
moduleFiles_ segments@(_crate : crateSegments) =
  [ joinPath segments <.> "rs",
    joinPath segments </> "mod" <.> "rs",
    joinPath crateSegments <.> "rs",
    joinPath crateSegments </> "mod" <.> "rs"
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> return ()
    moduleName : _ -> printGlobs $ moduleFiles moduleName

printGlobs :: [FilePath] -> IO ()
printGlobs [] = exitFailure
printGlobs (x : xs) = do
  globs <- glob x
  case globs of
    [] -> printGlobs xs
    (match : _) -> putStrLn match
