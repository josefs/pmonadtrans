{-# LANGUAGE GADTs #-}
module Control.Monad.Indexed.Trans.Writer where

import Prelude hiding (id,(.))

import Control.Monad.Indexed
import Control.Monad.Indexed.Trans
import Control.Monad.Indexed.Trans.TF

import Control.Category

data WriterIxT c m i j a where
  WriterIxT :: m (Snd i) (Snd j) (a,c (Fst i) (Fst j)) ->
               WriterIxT c m i j a 

instance (Category c, IxMonad m) => IxMonad (WriterIxT c m) where
  ireturn a = WriterIxT $ ireturn (a,id)
  WriterIxT m `ibind` f =
    WriterIxT $ m `ibind` \(a,w) ->
    case f a of
      WriterIxT m -> m `ibind` \(b,w') -> ireturn (b,w' . w)

instance Category c => IxMonadTrans (WriterIxT c) where
  ilift m = WriterIxT $ m `ibind` \a -> ireturn (a,id)

tell
  :: (IxMonad m, Snd j ~ Snd i) =>
     c (Fst i) (Fst j) -> WriterIxT c m i j ()
tell w = WriterIxT $ ireturn ((),w)

writer
  :: (IxMonad m, Snd j ~ Snd i) =>
     (a, c (Fst i) (Fst j)) -> WriterIxT c m i j a
writer p = WriterIxT $ ireturn p

listen
  :: IxMonad m =>
     WriterIxT c m i j a -> WriterIxT c m i j (a, c (Fst i) (Fst j))
listen (WriterIxT m) = WriterIxT $ m `ibind` \(a,w) -> ireturn ((a,w),w)

pass
  :: (IxMonad m, Snd i ~ Snd k, Snd j ~ Snd l) =>
     WriterIxT t m i j (a, t (Fst i) (Fst j) -> c (Fst k) (Fst l))
     -> WriterIxT c m k l a
pass (WriterIxT m) = WriterIxT $ m `ibind` \((a,p),w) -> ireturn (a,p w)
