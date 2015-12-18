{-# LANGUAGE GADTs #-}
module Control.Monad.Indexed.Id where

import Control.Monad.Indexed

data Id i j a where
  Id :: a -> Id i i a

instance IxMonad Id where
  ireturn a = Id a
  Id a `ibind` f = f a
