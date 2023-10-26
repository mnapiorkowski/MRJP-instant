module JVM.Backend where

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
  show (VConst i)
    | i <= 5 = "iconst_" ++ show i
    | i <= 127 = "bipush " ++ show i
    | i <= 32767 = "sipush " ++ show i
    | otherwise = "ldc " ++ show i
  show (VRef ref)
    | ref <= 3 = "_" ++ show ref
    | otherwise = " " ++ show ref

instance Show Op where
  show OAdd = "iadd"
  show OSub = "isub"
  show OMul = "imul"
  show ODiv = "idiv"

indentLine :: String -> String
indentLine line =
  if null line || ("." `isPrefixOf` line)
    then line ++ "\n"
  else "\t" ++ line ++ "\n"

indent :: Code -> String
indent = foldr ((++) . indentLine) ""

preamble :: String -> Code
preamble baseName = [
  ".source " ++ baseName ++ ".j",
  ".class public " ++ baseName,
  ".super java/lang/Object",
  "",
  ".method public <init>()V",
  "aload_0",
  "invokenonvirtual java/lang/Object/<init>()V",
  "return",
  ".end method",
  ""
  ]

header :: Int -> Int -> Code
header localsCount stackSize = [
  ".method public static main([Ljava/lang/String;)V",
  ".limit locals " ++ show localsCount,
  ".limit stack " ++ show stackSize
  ]

footer :: Code
footer = [
  "return",
  ".end method"
  ]

transBinOp :: Op -> Exp -> Exp -> CM Code
transBinOp op e1 e2 = do
  code1 <- transExp e1
  code2 <- transExp e2
  pure $ code1 ++ code2 ++ [show op]

transExp :: Exp -> CM Code
transExp e = case e of
  ExpAdd e1 e2 -> transBinOp OAdd e1 e2
  ExpSub e1 e2 -> transBinOp OSub e1 e2
  ExpMul e1 e2 -> transBinOp OMul e1 e2
  ExpDiv e1 e2 -> transBinOp ODiv e1 e2
  ExpLit int -> pure [show $ VConst int]
  ExpVar id -> do
    v <- getVarRef id
    case v of
      VRef ref -> pure ["iload" ++ show v]
      VConst i -> pure [show v]

transStmt :: Stmt -> CM Code
transStmt s = case s of
  SAss id e -> do
    code <- transExp e
    v <- newVar id
    pure $ code ++ ["istore" ++ show v]
  SExp e -> do
    code <- transExp e
    let getstatic = ["getstatic java/lang/System/out Ljava/io/PrintStream;"]
    let invokevirtual = ["invokevirtual java/io/PrintStream/println(I)V"]
    pure $ getstatic ++ code ++ invokevirtual

transStmts :: [Stmt]-> [Code] -> CM Code
transStmts [] acc = pure $ concat $ reverse acc
transStmts (h:t) acc = do
  s <- transStmt h
  transStmts t (s:acc)  

transProgr :: Program -> CM Code
transProgr (Prog stmts) = do
  code <- transStmts stmts []
  (_, localsCount) <- get
  return $ header localsCount 100 ++ code ++ footer

compileProgr :: Program -> String -> CM String
compileProgr p baseName = do
  code <- transProgr p
  pure $ indent (preamble baseName ++ code)

compile :: Program -> String -> IO String
compile p baseName = do
  let initEnv = (Map.empty, 0)
  let res = runExcept $ evalStateT (compileProgr p baseName) initEnv
  case res of
    Left err -> do
      hPutStrLn stderr $ "Compilation error: " ++ err
      exitFailure
    Right code -> return code
