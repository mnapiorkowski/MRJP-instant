module JVM.Backend where

import System.IO
import System.Exit (exitFailure)

import Control.Monad.State
import Control.Monad.Except

import Data.Map (Map)
import qualified Data.Map as Map

import Instant.Abs

import Common.Types

compile :: Program -> IO String
compile p = return ""