{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds  #-}
module Control.Monad.Indexed.Trans.Cont where

import Control.Monad.Indexed
import Control.Monad.Indexed.Trans
import Control.Monad.Indexed.Trans.TF

data ContIxT k m i j a where
  ContIxT :: { unContT :: (a -> m (Snd j) k  (Fst j)) -> m (Snd i) k (Fst i)
             } -> ContIxT k m i j a

instance IxMonad m => IxMonad (ContIxT k m) where
  ireturn a = ContIxT $ \k -> k a
  ContIxT m `ibind` f = ContIxT (\k -> m (\a -> unContT (f a) k))

instance IxMonadTrans (ContIxT k) where
  ilift m = ContIxT $ \k -> m `ibind` k

runContIxT :: IxMonad m => ContIxT j m '(s,i) '(t,j) t -> m i j s
runContIxT (ContIxT f) = f ireturn

callcc :: ((a -> ContIxT k m '(t,j) '(u,n) b) -> ContIxT k m '(s,i) '(t,j) a)
       -> ContIxT k m '(s,i) '(t,j) a
callcc f = ContIxT (\ c -> unContT (f (\ x -> ContIxT (\ _ -> c x))) c)

shift :: IxMonad m => ((a -> m j k t) -> ContIxT k m '(s,i) '(b,k) b)
                   -> ContIxT k m '(s,i) '(t,j) a
shift f = ContIxT (runContIxT . f)

reset :: IxMonad m => ContIxT j m '(a,i) '(t1,j) t1 -> ContIxT k m '(s,i) '(s,j) a
reset = ilift . runContIxT
