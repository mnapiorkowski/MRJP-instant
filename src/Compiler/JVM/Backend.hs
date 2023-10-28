module JVM.Backend where

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
indent = DList.foldr ((++) . indentLine) ""

preamble :: String -> Code
preamble baseName = DList.fromList [
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
header localsCount stackSize = DList.fromList [
  ".method public static main([Ljava/lang/String;)V",
  ".limit locals " ++ show localsCount,
  ".limit stack " ++ show stackSize
  ]

footer :: Code
footer = DList.fromList [
  "return",
  ".end method"
  ]

printInt :: Code
printInt = DList.fromList [
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

calcStackSize' :: [String] -> Int -> Int -> Int
calcStackSize' [] _ max = max
calcStackSize' (h:t) acc max = do
  let newStackSize = (acc + stackChange h)
  let newMax = if newStackSize > max then newStackSize else max
  calcStackSize' t newStackSize newMax

calcStackSize :: [String] -> Int
calcStackSize instrList = calcStackSize' instrList 0 0

transBinOp :: Op -> Exp -> Exp -> CM Code
transBinOp op e1 e2 = do
  code1 <- transExp e1
  code2 <- transExp e2
  pure $ DList.concat [code1, code2, DList.singleton (show op)]

transExp :: Exp -> CM Code
transExp e = case e of
  ExpAdd e1 e2 -> transBinOp OAdd e2 e1
  ExpSub e1 e2 -> transBinOp OSub e1 e2
  ExpMul e1 e2 -> transBinOp OMul e1 e2
  ExpDiv e1 e2 -> transBinOp ODiv e1 e2
  ExpLit int -> pure $ DList.singleton (show $ VConst int)
  ExpVar id -> do
    v <- getVar id
    case v of
      VRef ref -> pure $ DList.singleton ("iload" ++ show v)
      VConst i -> pure $ DList.singleton (show v)

transStmt :: Stmt -> CM Code
transStmt s = case s of
  SAss id e -> do
    code <- transExp e
    v <- getRef id
    pure $ DList.append code $ DList.singleton ("istore" ++ show v)
  SExp e -> do
    code <- transExp e
    pure $ DList.append code printInt

transStmts :: [Stmt]-> Code -> CM Code
transStmts [] acc = pure acc
transStmts (h:t) acc = do
  code <- transStmt h
  transStmts t (DList.append acc code)

transProgr :: Program -> CM Code
transProgr (Prog stmts) = do
  code <- transStmts stmts DList.empty
  (_, localsCount) <- get
  let maxStack = calcStackSize $ DList.toList code
  return $ DList.concat [(header (localsCount + 1) maxStack), code, footer]

compileProgr :: Program -> String -> CM String
compileProgr p baseName = do
  code <- transProgr p
  pure $ indent $ DList.append (preamble baseName) code

compile :: Program -> String -> IO String
compile p baseName = do
  let initEnv = (Map.empty, 0)
  let res = runExcept $ evalStateT (compileProgr p baseName) initEnv
  case res of
    Left err -> do
      hPutStrLn stderr $ "Compilation error: " ++ err
      exitFailure
    Right code -> return code
