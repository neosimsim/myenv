{-# OPTIONS_GHC -Wall -Werror #-}

import           Data.Function   ((&))
import           System.FilePath (FilePath)

data FileModification
  = Added { filePath :: FilePath }
  | Modified { filePath :: FilePath }
  | Renamed { oldPath  :: FilePath
            , filePath :: FilePath }
  | Deleted { filePath :: FilePath }
  deriving (Show, Eq)

readModification :: String -> FileModification
readModification line =
  case words line of
    ('M':_):fp:_      -> Modified fp
    ('R':_):fp1:fp2:_ -> Renamed fp1 fp2
    "D":fp:_          -> Deleted fp
    "A":fp:_          -> Added fp
    _                 -> error $ "can't parser " ++ line

somethingToLine :: FileModification -> String
somethingToLine (Added fp)      = "Added " ++ fp
somethingToLine (Renamed op fp) = "Renamed " ++ op ++ " " ++ fp
somethingToLine (Deleted fp)    = "Deleted " ++ fp
somethingToLine (Modified fp)   = "Modified " ++ fp

foo :: [FileModification] -> [FileModification]
foo = foldr addModification []

-- | Added and Deleted both overwrite Modified and eliminate each other.
-- assumption: normal order of git log, not reverse.
addModification :: FileModification -> [FileModification] -> [FileModification]
addModification x@(Added fp) xs =
  case span ((/= fp) . filePath) xs of
    (_, [])            -> x : xs
    (ys, Deleted _:zs) -> Modified fp : ys ++ zs
    (_, _:_)           -> error "file has been modified before added"
addModification x@(Deleted fp) xs =
  case span ((/= fp) . filePath) xs of
    (_, [])                  -> x : xs
    (ys, Added _:zs)         -> ys ++ zs
    (ys, Renamed oPath _:zs) -> Deleted oPath : ys ++ zs
    (ys, Modified _:zs)      -> x : ys ++ zs
    (_, Deleted _:_)         -> error "file has beed delted twice"
addModification x@(Renamed oPath newPath) xs =
  case span ((/= newPath) . filePath) xs of
    (_, []) ->
      case span ((/= oPath) . filePath) xs
         -- match on old name
            of
        (_, []) -> x : xs
        (_, Added _:_) -> Added newPath : xs
        (_, Deleted _:_) -> error "a deleted file has been renamed"
        (_, Renamed veryOldPath _:_) -> Renamed veryOldPath newPath : xs
        (_, Modified _:_) -> x : xs
    -- match on new name
    (ys, Deleted _:zs) -> Modified newPath : Deleted oPath : ys ++ zs
    (_, Added _:_) -> error "a file has been renamed to an added file"
    (_, Renamed _ _:_) -> error "two files have been renamed to the same file"
    (_, Modified _:_) ->
      error "file has been modefied before another got the same name"
addModification x@(Modified fp) xs =
  case span ((/= fp) . filePath) xs of
    (_, []) -> x : xs
    _       -> xs

main :: IO ()
main =
  getContents >>=
  (\x ->
     lines x & fmap readModification & foo & fmap somethingToLine &
     mapM_ putStrLn)
