module Main where

import System.IO
import System.Process (callProcess)

import Common.Frontend (readSource, analyse)
import LLVM.Backend (compile)

main :: IO ()
main = do
  (file, filePathNoExt) <- readSource
  progr <- analyse file
  code <- compile progr
  writeFile (filePathNoExt ++ ".ll") code
  out <- callProcess "llvm-as" [
    "-o", filePathNoExt ++ ".bc", 
    filePathNoExt ++ ".ll"
    ]
  return ()
