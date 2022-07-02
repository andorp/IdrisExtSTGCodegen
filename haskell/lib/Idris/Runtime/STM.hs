module Idris.Runtime.STM
  ( module Control.Concurrent.STM
  , stmReadTVar
  , stmBind
  , stmPure
  ) where

import Control.Concurrent.STM (STM, atomically, newTVar, readTVar)

stmReadTVar :: () -> TVar a -> STM a
stmReadTVar _ = readTVar

stmBind :: () -> () -> STM a -> (a -> STM b) -> STM b
stmBind _ _ = (>>=)

stmPure :: () -> a -> STM a
stmPure = pure

