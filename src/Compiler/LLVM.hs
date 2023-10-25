module Main where

import System.IO
import System.Process (callProcess)

import Common.Frontend (readSource, analyse)
import LLVM.Backend (compile)

main :: IO ()
main = do
  (file, basename) <- readSource
  progr <- analyse file
  code <- compile progr
  writeFile (basename ++ ".ll") code
  out <- callProcess "llvm-as" ["-o", basename ++ ".bc", basename ++ ".ll"]
  return ()
