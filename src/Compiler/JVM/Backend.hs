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

printInt :: Code
printInt = [
  "getstatic java/lang/System/out Ljava/io/PrintStream;",
  "swap",
  "invokevirtual java/io/PrintStream/println(I)V"
  ]

stackChange :: String -> Int
stackChange s
  | "iconst" `isPrefixOf` s = 1
  | "bipush" `isPrefixOf` s = 1
  | "sipush" `isPrefixOf` s = 1
  | "ldc" `isPrefixOf` s = 1
  | "iload" `isPrefixOf` s = 1
  | "istore" `isPrefixOf` s = -1
  | "iadd" `isPrefixOf` s = -1
  | "isub" `isPrefixOf` s = -1
  | "imul" `isPrefixOf` s = -1
  | "idiv" `isPrefixOf` s = -1
  | "getstatic" `isPrefixOf` s = 1
  | "invokevirtual" `isPrefixOf` s = -2
  | otherwise = 0

calcStackSize' :: Code -> Int -> Int -> Int
calcStackSize' [] _ max = max
calcStackSize' (h:t) acc max = do
  let newStackSize = (acc + stackChange h)
  let newMax = if newStackSize > max then newStackSize else max
  calcStackSize' t newStackSize newMax

calcStackSize :: Code -> Int
calcStackSize code = calcStackSize' code 0 0

transBinOp :: Op -> Exp -> Exp -> CM Code
transBinOp op e1 e2 = do
  code1 <- transExp e1
  code2 <- transExp e2
  pure $ code1 ++ code2 ++ [show op]

transExp :: Exp -> CM Code
transExp e = case e of
  ExpAdd e1 e2 -> transBinOp OAdd e2 e1
  ExpSub e1 e2 -> transBinOp OSub e1 e2
  ExpMul e1 e2 -> transBinOp OMul e1 e2
  ExpDiv e1 e2 -> transBinOp ODiv e1 e2
  ExpLit int -> pure [show $ VConst int]
  ExpVar id -> do
    v <- getVar id
    case v of
      VRef ref -> pure ["iload" ++ show v]
      VConst i -> pure [show v]

transStmt :: Stmt -> CM Code
transStmt s = case s of
  SAss id e -> do
    code <- transExp e
    v <- getRef id
    pure $ code ++ ["istore" ++ show v]
  SExp e -> do
    code <- transExp e
    pure $ code ++ printInt

transStmts :: [Stmt]-> [Code] -> CM Code
transStmts [] acc = pure $ concat $ reverse acc
transStmts (h:t) acc = do
  code <- transStmt h
  transStmts t (code:acc)

transProgr :: Program -> CM Code
transProgr (Prog stmts) = do
  code <- transStmts stmts []
  (_, localsCount) <- get
  let maxStack = calcStackSize code
  return $ (header (localsCount + 1) maxStack) ++ code ++ footer

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
