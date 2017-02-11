{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : Control.Monad.Param.Update

Update monad. See http://cs.ioc.ee/~tarmo/papers/types13.pdf
-}
module Control.Monad.Param.Update where

import Prelude hiding (id,(.))
import Control.Category
import Control.Arrow

import Control.Monad.Param

import Type.Pair

data UpdatePT c s m p q a where
  UpdatePT :: { runUpdatePT :: s -> m (Snd p) (Snd q) (c (Fst p) (Fst q), a) } -> UpdatePT c s m p q a

instance PFunctor m => PFunctor (UpdatePT c s m) where
  pfmap f (UpdatePT g) = UpdatePT $ pfmap (second f) . g

instance (Category c, PMonad m) => PApplicative (UpdatePT c s m) where
  ppure a = UpdatePT $ \_ -> ppure (id,a)
  UpdatePT m `papp` UpdatePT n = UpdatePT $ \s ->
    m s `pbind` \(c1,f) ->
    n s `pbind` \(c2,a) ->
    preturn (c2 . c1, f a)

instance (Action s c, PMonad m) => PMonad (UpdatePT c s m) where
  preturn a = UpdatePT $ \_ -> preturn (id,a)
  UpdatePT f `pbind` g =
    UpdatePT $ \s -> f s `pbind` \(c1,a) ->
                     runUpdatePT (g a) (action s c1) `pbind` \(c2,b) ->
                     preturn (c2 . c1, b)

class Category c => Action s c where
  action :: s -> c a b -> s

write :: (PMonad m, Snd p ~ Snd q) =>
         c (Fst p) (Fst q) -> UpdatePT c s m p q ()
write c = UpdatePT $ \_ -> preturn (c,())

env :: (Category c, PMonad m) => UpdatePT c s m p p s
env = UpdatePT $ \s -> preturn (id,s)
