module hello-world where

open import IO using (run; putStrLn)
import Agda.Builtin.IO as Builtin using (IO)
open import Data.Unit using (⊤)

main : Builtin.IO ⊤
main = run (putStrLn "Hello, World!")