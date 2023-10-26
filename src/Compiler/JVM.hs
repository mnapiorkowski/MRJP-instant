module Main where

import System.IO
import System.Process (callProcess)
import System.FilePath.Posix  (takeBaseName, takeDirectory)

import Common.Frontend (readSource, analyse)
import JVM.Backend (compile)

main :: IO ()
main = do
  (file, filePathNoExt) <- readSource
  let baseName = takeBaseName filePathNoExt
  let path = takeDirectory filePathNoExt
  progr <- analyse file
  code <- compile progr baseName
  writeFile (filePathNoExt ++ ".j") code
  out <- callProcess "java" [
    "-jar", "lib/jasmin.jar", 
    "-d", path, 
    filePathNoExt ++ ".j"
    ]
  return ()
