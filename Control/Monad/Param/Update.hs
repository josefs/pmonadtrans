{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Param.Update where

import Prelude hiding (id,(.))
import Control.Category
import Control.Arrow

import Control.Monad.Param

data UpdateP c s p q a where
  UpdateP :: { runUpdateP :: s -> (c p q, a) } -> UpdateP c s p q a

instance PFunctor (UpdateP c s) where
  pfmap f (UpdateP g) = UpdateP $ second f . g

instance Category c => PApplicative (UpdateP c s) where
  ppure a = UpdateP $ \_ -> (id,a)
  UpdateP m `papp` UpdateP n = UpdateP $ \s ->
    let (c1,f) = m s
        (c2,a) = n s
    in (c2 . c1, f a)

instance (Action s c) => PMonad (UpdateP c s) where
  preturn a = UpdateP $ \_ -> (id,a)
  UpdateP f `pbind` g = UpdateP $ \s -> let (c1,a) = f s
                                            (c2,b) = runUpdateP (g a) (action s c1)
                                        in (c2 . c1, b)

class Category c => Action s c where
  action :: s -> c a b -> s
