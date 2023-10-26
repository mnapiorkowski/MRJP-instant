module Common.Types where

import Control.Monad.Except
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import Instant.Abs (Ident)

type Code = [String]

type Loc = Int
type CState = (Map Ident Loc, Loc)
type CM a = StateT CState (Except String) a

data Val = VConst Integer | VRef Loc
data Op = OAdd | OSub | OMul | ODiv