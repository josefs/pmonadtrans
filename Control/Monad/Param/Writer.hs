{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.Param.Writer where

import Prelude hiding (id,(.))

import Type.Pair
import Control.Monad.Param
import Control.Monad.Param.Trans

import Control.Category

import GHC.Exts

data WriterPT c m i j a where
  WriterPT :: { runWriterPT :: m (Snd i) (Snd j) (a,c (Fst i) (Fst j)) } ->
               WriterPT c m i j a 

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

tell
  :: (PMonad m, Snd j ~ Snd i) =>
     c (Fst i) (Fst j) -> WriterPT c m i j ()
tell w = WriterPT $ preturn ((),w)

writer
  :: (PMonad m, Snd j ~ Snd i) =>
     (a, c (Fst i) (Fst j)) -> WriterPT c m i j a
writer p = WriterPT $ preturn p

listen
  :: PMonad m =>
     WriterPT c m i j a -> WriterPT c m i j (a, c (Fst i) (Fst j))
listen (WriterPT m) = WriterPT $ m `pbind` \(a,w) -> preturn ((a,w),w)

pass
  :: (PMonad m, Snd i ~ Snd k, Snd j ~ Snd l) =>
     WriterPT t m i j (a, t (Fst i) (Fst j) -> c (Fst k) (Fst l))
     -> WriterPT c m k l a
pass (WriterPT m) = WriterPT $ m `pbind` \((a,p),w) -> preturn (a,p w)

type family Cat (m :: k -> k -> * -> *) :: k' -> k' -> *
type family Acc (m :: l -> l -> * -> *) (a :: k) :: k'
type family NotAccEq (m :: k -> k -> * -> *) (a :: k) (b :: k) :: Constraint

class Writer (m :: k -> k -> * -> *) where
{-
  type Cat m :: k' -> k' -> *
  type Acc m (a :: k) :: k'
  type NotAccEq m (a :: k) (b :: k) :: Constraint
-}
  tellW :: NotAccEq m a b => (Cat m) (Acc m a) (Acc m b) -> m a b ()

type instance Cat (WriterPT c m) = c
type instance Acc (WriterPT c m) a = Fst a
type instance NotAccEq (WriterPT c m) a b = Snd a ~ Snd b
{-
instance PMonad m => Writer (WriterPT c m) where
  {-
  type Cat (WriterPT c m) = c
  type Acc (WriterPT c m) a = Fst a
  type NotAccEq (WriterPT c m) a b = Snd a ~ Snd b
-}
  tellW c = WriterPT (preturn ((),c))
-}
{-
data WriterM c i j a where
  WriterM :: { runWriterM :: (a,c i j) } -> WriterM c i j a

type instance Cat (WriterM c) = c
type instance Acc (WriterM c) a = a

type instance NotAccEq (WriterM c) a b = ()

--instance Writer (WriterM c) where
--  tellW c = WriterM ((),c)


class Writer' m c | m -> c where
  tellW' :: (NotAccEq m a b, Acc m i ~ a, Acc m j ~ b) => c i j -> m a b ()

instance Writer' (WriterM c) c where
  tellW' c = WriterM ((),c)

instance Writer' (WriterPT c m) c where
  tellW' c = WriterPT (preturn ((),c))
-}
