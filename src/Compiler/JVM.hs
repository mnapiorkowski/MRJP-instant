module Main where

import System.IO
import System.Process (callProcess)

import Common.Frontend (readSource, analyse)
import JVM.Backend (compile)

main :: IO ()
main = do
  (file, basename) <- readSource
  progr <- analyse file
  code <- compile progr
  writeFile (basename ++ ".j") code
  out <- callProcess "java" ["-jar", "lib/jasmin.jar", basename ++ ".j"]
  return ()
