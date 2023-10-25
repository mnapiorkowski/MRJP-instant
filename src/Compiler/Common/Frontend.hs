module Common.Frontend where

import System.IO
import System.Environment     (getArgs)
import System.FilePath.Posix  (dropExtension)
import System.Exit            (exitFailure)

import Control.Monad.Except

import Instant.Abs  (Program)
import Instant.Par  (pProgram, myLexer)

parse :: String -> Either String Program
parse s = pProgram $ myLexer s

analyse :: String -> IO Program
analyse file = do
  case parse file of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right progr -> return progr

readSource :: IO (String, String)
readSource = do
  args <- getArgs
  if length args /= 1
    then do
      hPutStrLn stderr $ "Usage: ./compiler <source file>"
      exitFailure
  else do
    let path = args !! 0
    file <- readFile $ path
    let basename = dropExtension path
    return (file, basename)
