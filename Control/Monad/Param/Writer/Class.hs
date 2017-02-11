{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
module Control.Monad.Param.Writer.Class where

import Control.Monad.Param

import GHC.Exts

class PMonad m => Writer (m :: k -> k -> * -> *) where
  type Cat (m :: k -> k -> * -> *) :: * -> * -> *
  type Acc (m :: k -> k -> * -> *) (a :: k) :: *
  type NotAccEq (m :: k -> k -> * -> *) (a :: k) (b :: k) :: Constraint
  tellW :: NotAccEq m a b => (Cat m) (Acc m a) (Acc m b) -> m a b ()

