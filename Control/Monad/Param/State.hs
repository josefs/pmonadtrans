{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Param.State where

import Prelude hiding ((.),id)

import Control.Monad.Param
import Control.Monad.Param.Trans
import Control.Monad.Param.Id

import Control.Category

import Type.Pair

import GHC.Exts

data StatePT :: (k -> k -> * -> *) -> (*,k) -> (*,k) -> * -> * where
     StatePT :: { runStatePT :: (Fst i -> ixm (Snd i) (Snd j) (a,Fst j)) } ->
                StatePT ixm i j a

instance PFunctor f => PFunctor (StatePT f) where
  pfmap f (StatePT m) = StatePT $ \s -> pfmap (\(a,s') -> (f a,s')) (m s)

instance PMonad m => PApplicative (StatePT m) where
  ppure a = StatePT $ \s -> ppure (a,s)
  StatePT m `papp` StatePT n =
    StatePT $ \s -> m s  `pbind` \(f,s') ->
                    n s' `pbind` \(a,s'') ->
                    preturn (f a, s'')

instance PMonad m => PMonad (StatePT m) where
  preturn a = StatePT $ \s -> preturn (a,s)
  StatePT m `pbind` f =
     StatePT $ \s -> m s `pbind` \(a,t) ->
                      case f a of
                        StatePT g -> g t

instance PMonadTrans StatePT where
  plift m = StatePT $ \s -> m `pbind` \a -> preturn (a,s)

type instance GetState (StatePT m) s = Fst s
type instance NotStateEq (StatePT m) i j = Snd j ~ Snd i

instance PMonad m => State (StatePT m) where
  get = StatePT $ \s -> preturn (s,s)
  set s = StatePT $ \_ -> preturn ((),s)

instance PMonadFix m => PMonadFix (StatePT m) where
  pfix f = StatePT $ \s -> pfix (\ ~(a,_) -> runStatePT (f a) s)

class State (m :: k -> k -> * -> *) where
  get :: m s s (GetState m s)
  set :: NotStateEq m s' s =>
         GetState m s -> m s' s ()

type family GetState (m :: k -> k -> * -> *) (a :: k) :: *
type family NotStateEq (m :: k -> k -> * -> *) (a :: k) (b :: k) :: Constraint

type StateP = StatePT IdP
