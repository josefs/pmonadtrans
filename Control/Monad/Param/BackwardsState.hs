{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Control.Monad.Param.BackwardsState where

import Control.Monad.Param
import Control.Monad.Param.Trans

import Type.Pair

import Control.Arrow

data BStateP i j a where
  BStateP :: { unBStateP :: j -> (a,i) } -> BStateP i j a

instance PFunctor BStateP where
  pfmap f (BStateP g) = BStateP (first f . g)

instance PApplicative BStateP where
  ppure a = BStateP $ \s -> (a,s)
  BStateP m `papp` BStateP n = {- BStateP $ \s ->
    let (f,s'') = m s'
        (a,s')  = n s
    in (f a, s'')
-}
                               BStateP $ \s ->
                                 let ((f,s''),(a,s')) =
                                       fix (\ ((f,s''),(a,s')) ->
                                             (m s', n s))
                                 in (f a, s'')

instance PMonad BStateP where
  preturn a = BStateP $ \s -> (a,s)
  BStateP m `pbind` f = {- BStateP $ \s ->
                           let (a,s'') = m s'
                               (b,s')  = unBStateP (f a) s
                           in (b,s'')
-}
                         BStateP $ \s ->
                           let ((a,s''),(b,s')) = 
                                  fix (\ ((a,s''),(b,s')) ->
                                         (m s',unBStateP (f a) s))
                           in (b,s'')

fix f = let x = f x in x

data BStatePT m i j a where
  BStatePT :: { unBStatePT :: Fst j -> m (Snd i) (Snd j) (a,Fst i) }
   -> BStatePT m i j a

instance PFunctor m => PFunctor (BStatePT m) where
  pfmap f (BStatePT g) = BStatePT $ pfmap (first f) . g

instance PMonad m => PApplicative (BStatePT m) where
  ppure a = BStatePT $ \s -> ppure (a,s)
  BStatePT m `papp` BStatePT n = BStatePT $ \s ->
    pfix (\ ~(_,(_,s')) ->
           m s' `pbind` \(f,s'') ->
           n s  `pbind` \(a,s')  ->
           preturn ((f,s''),(a,s')))
    `pbind` \((f,s''),(a,_)) ->
    preturn (f a,s'')

instance PMonadTrans BStatePT where
  plift m = BStatePT $ \s -> m `pbind` \a -> preturn (a,s)

instance PMonadFix m => PMonad (BStatePT m) where
  preturn a = BStatePT $ \s -> preturn (a,s)
  BStatePT m `pbind` f = BStatePT $ \s ->
                            (pfix (\ ~((a,s''),(b,s')) ->
                              m s' `pbind` \(a,s'') ->
                              unBStatePT (f a) s `pbind` \(b,s') ->
                              preturn ((a,s''),(b,s'))))
                            `pbind` \((a,s''),(b,s')) ->
                            preturn (b,s'')
