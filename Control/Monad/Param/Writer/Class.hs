{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
module Control.Monad.Param.Writer.Class where

import Control.Monad.Param

import Type.Pair

import GHC.Exts

class PMonad m => PMonadWriter (m :: k -> k -> * -> *) where
  type Cat (m :: k -> k -> * -> *) :: * -> * -> *
  type Acc (m :: k -> k -> * -> *) (a :: k) :: *
  type NotAccEq (m :: k -> k -> * -> *) (a :: k) (b :: k) :: Constraint
  tell   :: NotAccEq m i j => (Cat m) (Acc m i) (Acc m j) -> m i j ()
  listen :: m i j a -> m i j (a, (Cat m) (Acc m i) (Acc m j))
