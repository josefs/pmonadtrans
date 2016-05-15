{-# LANGUAGE GADTs #-}
module Control.Monad.Param.Id where

import Control.Monad.Param

data IdP i j a where
  IdP :: a -> IdP i i a

instance PMonad IdP where
  preturn a = IdP a
  IdP a `pbind` f = f a

instance PFunctor IdP where
  pfmap f (IdP a) = IdP (f a)

instance PApplicative IdP where
  ppure a = IdP a
  papp (IdP f) (IdP a) = IdP (f a)

instance PMonadFix IdP where
  pfix f = IdP $ let IdP a = f a in a
