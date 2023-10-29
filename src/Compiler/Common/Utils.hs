module Common.Utils where

import Control.Monad.State
import Control.Monad.Except

import Data.Map (Map)
import qualified Data.Map as Map

import Instant.Abs

import Common.Types

throwE :: String -> CM a
throwE s = lift $ throwError s

newRef :: CM Val
newRef = do
  (state, newLoc) <- get
  put $ (state, succ newLoc)
  pure $ VRef newLoc

newVar :: Ident -> CM Val
newVar id = do
  (state, newLoc) <- get
  put $ (Map.insert id newLoc state, succ newLoc)
  pure $ VRef newLoc

getRef :: Ident -> CM Val
getRef id = do
  (state, newLoc) <- get
  if Map.notMember id state
    then newVar id
  else pure $ VRef (state Map.! id)

getVar :: Ident -> CM Val
getVar id@(Ident name) = do
  (state, _) <- get
  if Map.notMember id state
    then throwE $ "undefined variable " ++ name
  else pure $ VRef (state Map.! id)
  