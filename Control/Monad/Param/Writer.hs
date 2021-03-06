{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}
module Control.Monad.Param.Writer where

import Prelude hiding (id,(.))

import Type.Pair
import Control.Monad.Param
import Control.Monad.Param.Trans

import Control.Monad.Param.Writer.Class
import Control.Monad.Param.State.Class

import Control.Category

import GHC.Exts

data WriterPT c m i j a where
  WriterPT :: { runWriterPT :: m (Snd i) (Snd j)
                                 (a,c (Fst i :: *) (Fst j :: *)) } ->
               WriterPT c m i j a 
-- I'd rather avoid forcing the kind of c to be * -> * -> *
-- but I don't know how to make a monad transformer when it is
-- kind polymorphic


instance PFunctor f => PFunctor (WriterPT c f) where
  pfmap f (WriterPT m) = WriterPT $ pfmap (\(a,c) -> (f a,c)) m

instance (PApplicative m, Category c)
      => PApplicative (WriterPT c m) where
  ppure a = WriterPT $ ppure (a,id)
  WriterPT m `papp` WriterPT n = WriterPT $
    ppure (\(f,c1) (a,c2) -> (f a, c2 . c1)) `papp` m `papp` n

instance (Category c, PMonad m) => PMonad (WriterPT c m) where
  preturn a = WriterPT $ preturn (a,id)
  WriterPT m `pbind` f =
    WriterPT $ m `pbind` \(a,w) ->
    case f a of
      WriterPT m -> m `pbind` \(b,w') -> preturn (b,w' . w)

instance Category c => PMonadTrans (WriterPT c) where
  plift m = WriterPT $ m `pbind` \a -> preturn (a,id)

instance (PMonadFix m, Category c) => PMonadFix (WriterPT c m) where
  pfix f = WriterPT $ pfix (\ ~(a,_) -> runWriterPT $ f a)

writer
  :: (PMonad m, Snd j ~ Snd i) =>
     (a, c (Fst i) (Fst j)) -> WriterPT c m i j a
writer p = WriterPT $ preturn p

pass
  :: (PMonad m, Snd i ~ Snd k, Snd j ~ Snd l) =>
     WriterPT t m i j (a, t (Fst i) (Fst j) -> c (Fst k) (Fst l))
     -> WriterPT c m k l a
pass (WriterPT m) = WriterPT $ m `pbind` \((a,p),w) -> preturn (a,p w)

instance (PMonad m, Category c) => PMonadWriter (WriterPT c m) where
  type Cat (WriterPT c m) = c
  type Acc (WriterPT c m) a = Fst a
  type NotAccEq (WriterPT c m) a b = Snd a ~ Snd b
  tell c = WriterPT (preturn ((),c))
  listen (WriterPT m) = WriterPT $ m `pbind` \(a,w) -> preturn ((a,w),w)

instance (PMonadState m, PMonad m, Category c) => PMonadState (WriterPT c m) where
  get = WriterPT (get `pbind` \s -> preturn (s,id))
  set s = WriterPT (set s `pbind` \_ -> preturn ((),id))

type instance GetState   (WriterPT c m) a   = GetState m (Snd a)
type instance NotStateEq (WriterPT c m) a b = (Fst a ~ Fst b
                                              ,NotStateEq m (Snd a) (Snd b))
