{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Indexed.Trans.StateNice where

import Prelude hiding ((.),id)

import Control.Monad.Indexed
import Control.Monad.Indexed.Trans
import Control.Monad.Indexed.Trans.TF

import Control.Category

import GHC.Exts

data StateIxT :: (k -> k -> * -> *) -> (*,k) -> (*,k) -> * -> * where
  StateIxT :: (i -> ixm j l (a,k)) ->
              StateIxT ixm '(i,j) '(k,l) a

instance IxMonadTrans StateIxT where
  ilift m = StateIxT $ \s -> m `ibind` \a -> ireturn (a,s)

instance IxMonad m => IxMonad (StateIxT m) where
  ireturn a = StateIxT $ \s -> ireturn (a,s)
  StateIxT m `ibind` f =
     StateIxT $ \s -> m s `ibind` \(a,t) ->
                      case f a of
                        StateIxT g -> g t
{-
data WriterIxT c m i j a where
  WriterIxT :: { unWriter :: m (Snd i) (Snd j) (c (Fst i) (Fst j),a) }
    -> WriterIxT c m i j a

class State (m :: k -> k -> * -> *) where
  get :: m s s (GetState m s)
  set :: NotStateEq m s' s =>
         GetState m s -> m s' s ()

type family GetState (m :: k -> k -> * -> *) (a :: k) :: *
type family NotStateEq (m :: k -> k -> * -> *) (a :: k) (b :: k) :: Constraint

type instance GetState (StateIxT m) s = Fst s
type instance NotStateEq (StateIxT m) i j = Snd j ~ Snd i

instance IxMonad m => State (StateIxT m) where
  get = StateIxT $ \s -> ireturn (s,s)
  set s = StateIxT $ \_ -> ireturn ((),s)

type instance GetState (WriterIxT c m) s = GetState m (Snd s)
type instance NotStateEq (WriterIxT c m) i j =
  (Fst j ~ Fst i, NotStateEq m (Snd i) (Snd j))

instance (State m,Category c,IxMonad m) => State (WriterIxT c m) where
  get = WriterIxT $ get `ibind` \s -> ireturn (id,s)
  set s = WriterIxT $ set s `ibind` \() -> ireturn (id,())

data StateIx :: * -> * -> * -> * where
  StateIx :: { unStateIx :: (i -> (a,j)) } -> StateIx i j a

type instance GetState StateIx s = s
type instance NotStateEq StateIx i j = ()

instance State StateIx where
  get = StateIx $ \s -> (s,s)
  set s = StateIx $ \_ -> ((),s)

instance IxMonad StateIx where
  ireturn a = StateIx $ \s -> (a,s)
  StateIx m `ibind` f = StateIx $ \s -> let (a,s') = m s
                                        in unStateIx (f a) s'

instance IxMonadFix StateIx where
  ifix f = StateIx $ \s -> let (a,s') = unStateIx (f a) s in (a,s')
{-
instance IxMonadFix m => IxMonadFix (StateIxT m) where
  ifix f = StateIxT $ \s -> 
-}
-}
