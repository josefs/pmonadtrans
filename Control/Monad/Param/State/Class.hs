{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
module Control.Monad.Param.State.Class where

import Control.Monad.Param

import GHC.Exts

class PMonad m => State (m :: k -> k -> * -> *) where
  get :: m s s (GetState m s)
  set :: NotStateEq m s' s =>
         GetState m s -> m s' s ()

type family GetState   (m :: k -> k -> * -> *) (a :: k) :: *
type family NotStateEq (m :: k -> k -> * -> *) (a :: k) (b :: k) :: Constraint
