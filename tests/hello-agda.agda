module hello-agda where

open import IO using (run; putStrLn)
import Agda.Builtin.IO as Builtin using (IO)
open import Data.Unit.Polymorphic.Base using (⊤)
open import Agda.Primitive using (lzero)

main : Builtin.IO {lzero} ⊤
main = run (putStrLn "Hello, Agda!")