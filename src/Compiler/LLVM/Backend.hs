module LLVM.Backend where

import System.IO
import System.Exit (exitFailure)

import Control.Monad.State
import Control.Monad.Except

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (isPrefixOf)

import Instant.Abs

import Common.Types
import Common.Utils

instance Show Val where
  show (VConst i) = show i
  show (VRef ref) = "%_" ++ show ref

instance Show Op where
  show OAdd = "add i32"
  show OSub = "sub i32"
  show OMul = "mul i32"
  show ODiv = "div i32"

indentLine :: String -> String
indentLine line =
  if null line ||
     any (`isPrefixOf` line) ["@", "}", "declare", "define"] 
  then line ++ "\n"
  else "\t" ++ line ++ "\n"

indent :: Code -> String
indent = foldr ((++) . indentLine) ""

preamble :: Code
preamble = [
  "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"",
  "",
  "declare i32 @printf(i8*, ...) ",
  "",
  "define void @printInt(i32 %x) {",
  "%t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0",
  "call i32 (i8*, ...) @printf(i8* %t0, i32 %x)",
  "ret void",
  "}",
  ""
  ]

header :: Code
header = [
  "define i32 @main() {"
  ]

footer :: Code
footer = [
  "ret i32 0",
  "}"
  ]

transBinOp :: Op -> Exp -> Exp -> CM (Code, Val)
transBinOp op e1 e2 = do
  (code1, v1) <- transExp e1
  (code2, v2) <- transExp e2
  ref <- newRef
  let args = show v1 ++ ", " ++ show v2
  let codeOp = [show ref ++ " = " ++ show op ++ " " ++ args]
  pure (code1 ++ code2 ++ codeOp, ref)

transExp :: Exp -> CM (Code, Val)
transExp e = case e of
  ExpAdd e1 e2 -> transBinOp OAdd e1 e2
  ExpSub e1 e2 -> transBinOp OSub e1 e2
  ExpMul e1 e2 -> transBinOp OMul e1 e2
  ExpDiv e1 e2 -> transBinOp ODiv e1 e2
  ExpLit int -> pure ([], VConst int)
  ExpVar id -> do
    ref <- getVarRef id
    pure ([], ref)

transStmt :: Stmt -> CM Code
transStmt s = case s of
  SAss id e -> do
    (code, v) <- transExp e
    ref <- newVar id
    pure $ code ++ [show ref ++ " = " ++ show OAdd ++ " " ++ show v ++ ", 0"]
  SExp e -> do
    (code, v) <- transExp e
    pure $ code ++ ["call void @printInt(i32 " ++ show v ++ ")"]

transStmts :: [Stmt]-> [Code] -> CM Code
transStmts [] acc = pure $ concat $ reverse acc
transStmts (h:t) acc = do
  s <- transStmt h
  transStmts t (s:acc)  

transProgr :: Program -> CM Code
transProgr (Prog stmts) = do
  transStmts stmts []

compileProgr :: Program -> CM String
compileProgr p = do
  code <- transProgr p
  pure $ indent (preamble ++ header ++ code ++ footer)

compile :: Program -> IO String
compile p = do
  let initEnv = (Map.empty, 0)
  let res = runExcept $ evalStateT (compileProgr p) initEnv
  case res of
    Left err -> do
      hPutStrLn stderr $ "Compilation error: " ++ err
      exitFailure
    Right code -> return code