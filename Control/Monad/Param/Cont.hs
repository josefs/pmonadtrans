{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds  #-}
module Control.Monad.Param.Cont where

import Type.Pair
import Control.Monad.Param
import Control.Monad.Param.Trans

{- I can't say I understand this type. In particular I don't understand
   why the second parameter of 'm' can be anything but the first one needs
   to be something very particular.

   Well, thinking about it a little here's one way to interpret it.
   The continuation is what is going to happen after the current computation,
   so therefore the post-state of the current computation is the pre-state
   of the continuation. And the continuation runs all the way to the end
   and the end state is k. Now the monad computation we return it the thing
   that is going to the run right now and hence it must accept the current
   pre-state as a pre-state. And it will run until the end of time which is
   why it has k as it post-state.
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

callcc :: ((a -> ContPT k m '(t,j) '(u,n) b) -> ContPT k m '(s,i) '(t,j) a)
       -> ContPT k m '(s,i) '(t,j) a
callcc f = ContPT (\ c -> unContT (f (\ x -> ContPT (\ _ -> c x))) c)

shift :: PMonad m => ((a -> m j k t) -> ContPT k m '(s,i) '(b,k) b)
                   -> ContPT k m '(s,i) '(t,j) a
shift f = ContPT (runContPT . f)

reset :: PMonad m => ContPT j m '(a,i) '(t,j) t -> ContPT k m '(s,i) '(s,j) a
reset = plift . runContPT
