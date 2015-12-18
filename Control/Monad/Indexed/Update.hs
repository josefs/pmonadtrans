{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Indexed.Update where

{- An attempt at an indexed update monad
-}

import Prelude hiding (id,(.))
import Control.Category

import Control.Monad.Indexed

data UpdateIx c s p q a where
  UpdateIx :: { runUpdateIx :: s -> (c p q, a) } -> UpdateIx c s p q a

instance (Action s c) => IxMonad (UpdateIx c s) where
  ireturn a = UpdateIx $ \_ -> (id,a)
  UpdateIx f `ibind` g = UpdateIx $ \s -> let (c1,a) = f s
                                              (c2,b) = runUpdateIx (g a) (action s c1)
                                          in (c2 . c1, b)

class Category c => Action s c where
  action :: s -> c a b -> s
