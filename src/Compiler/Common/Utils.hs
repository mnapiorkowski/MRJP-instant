module Common.Utils where

import Control.Monad.State
import Control.Monad.Except

import Data.Map (Map)
import qualified Data.Map as Map

import Instant.Abs

import Common.Types

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

getVarRef :: Ident -> CM Val
getVarRef id = do
  (state, _) <- get
  if Map.notMember id state
    then pure $ VConst 0
  else pure $ VRef (state Map.! id)