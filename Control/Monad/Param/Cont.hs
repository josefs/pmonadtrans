{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE PolyKinds  #-}
module Control.Monad.Param.Cont where

import Type.Pair
import Control.Monad.Param
import Control.Monad.Param.Trans

import Control.Monad.Param.Cont.Class

{- Here's one way to interpret this very complicated type.
   The continuation is what is going to happen after the current
   computation, so therefore the post-state of the current computation
   is the pre-state of the continuation. And the continuation runs all
   the way to the end and the end state is k. Now the monad computation
   we return it she thing that is going to run right now and hence
   it must accept the current pre-state as a pre-state. And it will
   run until the end of time which is why it has k as it post-state.
-}

data ContPT k m i j a where
  ContPT :: { unContT :: (a -> m (Snd j) k (Fst j)) -> m (Snd i) k (Fst i)
             } -> ContPT k m i j a

instance PFunctor (ContPT k m) where
  pfmap f (ContPT m) = ContPT $ \k -> m (k . f)

instance PApplicative (ContPT k m) where
  ppure a = ContPT $ \k -> k a
  ContPT m `papp` ContPT n = ContPT $ \k ->
    m (\f -> n (\a -> k (f a)))

instance PMonad (ContPT k m) where
  preturn a = ContPT $ \k -> k a
  ContPT m `pbind` f = ContPT (\k -> m (\a -> unContT (f a) k))

instance PMonadTrans (ContPT k) where
  plift m = ContPT $ \k -> m `pbind ` k

runContPT :: PMonad m => ContPT j m '(s,i) '(t,j) t -> m i j s
runContPT (ContPT f) = f preturn

callccT :: ((a -> ContPT k m j n b) -> ContPT k m i j a)
        -> ContPT k m i j a
callccT f = ContPT (\ c -> unContT (f (\ x -> ContPT (\ _ -> c x))) c)

shift :: PMonad m => ((a -> m j k t) -> ContPT k m '(s,i) '(b,k) b)
                   -> ContPT k m '(s,i) '(t,j) a
shift f = ContPT (runContPT . f)

reset :: PMonad m => ContPT j m '(a,i) '(t,j) t -> ContPT k m '(s,i) '(s,j) a
reset = plift . runContPT

instance PMonadCont (ContPT k m) where
  callcc = callccT

