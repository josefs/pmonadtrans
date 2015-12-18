{-# LANGUAGE GADTs #-}
module Control.Monad.Indexed.BackwardsState where

import Control.Monad.Indexed
import Control.Monad.Indexed.Trans
import Control.Monad.Indexed.Trans.TF

data BStateIx i j a where
  BStateIx :: { unBStateIx :: j -> (a,i) } -> BStateIx i j a

instance IxMonad BStateIx where
  ireturn a = BStateIx $ \s -> (a,s)
  BStateIx m `ibind` f = {- BStateIx $ \s ->
                           let (a,s'') = m s'
                               (b,s')  = unBStateIx (f a) s
                           in (b,s'')
-}
                         BStateIx $ \s ->
                           let ((a,s''),(b,s')) = 
                                  fix (\ ((a,s''),(b,s')) ->
                                         (m s',unBStateIx (f a) s))
                           in (b,s'')

fix f = let x = f x in x

data BStateIxT m i j a where
  BStateIxT :: { unBStateIxT :: Fst j -> m (Snd i) (Snd j) (a,Fst i) }
   -> BStateIxT m i j a

instance IxMonadTrans BStateIxT where
  ilift m = BStateIxT $ \s -> m `ibind` \a -> ireturn (a,s)

instance IxMonadFix m => IxMonad (BStateIxT m) where
  ireturn a = BStateIxT $ \s -> ireturn (a,s)
  BStateIxT m `ibind` f = BStateIxT $ \s -> ifix (\ ~((a,s''),(b,s')) ->
                            m s' `ibind` \(a,s'') ->
                            unBStateIxT (f a) s `ibind` \(b,s') ->
                            ireturn ((a,s''),(b,s'))) `ibind` \((a,s''),(b,s')) -> 
                            ireturn (b,s'')

