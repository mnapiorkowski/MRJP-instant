module LLVM.Backend where

import System.IO
import System.Exit (exitFailure)

import Control.Monad.State
import Control.Monad.Except

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (isPrefixOf)
import qualified Data.DList as DList

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
  show ODiv = "sdiv i32"

indentLine :: String -> String
indentLine line =
  if null line ||
     any (`isPrefixOf` line) ["@", "}", "declare", "define"] 
  then line ++ "\n"
  else "\t" ++ line ++ "\n"

indent :: Code -> String
indent = DList.foldr ((++) . indentLine) ""

preamble :: Code
preamble = DList.fromList [
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
header = DList.fromList [
  "define i32 @main() {"
  ]

footer :: Code
footer = DList.fromList [
  "ret i32 0",
  "}"
  ]

transBinOp :: Op -> Exp -> Exp -> CM (Code, Val)
transBinOp op e1 e2 = do
  (code1, v1) <- transExp e1
  (code2, v2) <- transExp e2
  ref <- newRef
  let args = show v1 ++ ", " ++ show v2
  let codeOp = DList.singleton (show ref ++ " = " ++ show op ++ " " ++ args)
  pure (DList.concat [code1, code2, codeOp], ref)

transExp :: Exp -> CM (Code, Val)
transExp e = case e of
  ExpAdd e1 e2 -> transBinOp OAdd e1 e2
  ExpSub e1 e2 -> transBinOp OSub e1 e2
  ExpMul e1 e2 -> transBinOp OMul e1 e2
  ExpDiv e1 e2 -> transBinOp ODiv e1 e2
  ExpLit int -> pure (DList.empty, VConst int)
  ExpVar id -> do
    ref <- getVar id
    pure (DList.empty, ref)

transStmt :: Stmt -> CM Code
transStmt s = case s of
  SAss id e -> do
    (code, v) <- transExp e
    ref <- newVar id
    let codeAss = (show ref ++ " = " ++ show OAdd ++ " " ++ show v ++ ", 0")
    pure $ DList.append code (DList.singleton codeAss)
  SExp e -> do
    (code, v) <- transExp e

    let codePrint = ("call void @printInt(i32 " ++ show v ++ ")")
    pure $ DList.append code (DList.singleton codePrint)

transStmts :: [Stmt]-> Code -> CM Code
transStmts [] acc = pure acc
transStmts (h:t) acc = do
  code <- transStmt h
  transStmts t (DList.append acc code)

transProgr :: Program -> CM Code
transProgr (Prog stmts) = do
  transStmts stmts DList.empty

compileProgr :: Program -> CM String
compileProgr p = do
  code <- transProgr p
  pure $ indent $ DList.concat [preamble, header, code, footer]

compile :: Program -> IO String
compile p = do
  let initEnv = (Map.empty, 0)
  let res = runExcept $ evalStateT (compileProgr p) initEnv
  case res of
    Left err -> do
      hPutStrLn stderr $ "Compilation error: " ++ err
      exitFailure
    Right code -> return code