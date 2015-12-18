{-# LANGUAGE GADTs #-}
module Control.Monad.Indexed.Cont where

import Control.Monad.Indexed

data ContIx r o a where
  ContIx :: { unCont :: (a -> o) -> r } -> ContIx r o a

instance IxMonad ContIx where
  ireturn a = ContIx $ \k -> k a
  ContIx f `ibind` g = ContIx $ \k -> f $ \a -> unCont (g a) k

data ContIxT m i j k l o r a where
  ContIxT :: { unContT :: (a -> m i j o) -> m k l r } ->
    ContIxT m i j k l o r a

ret a = ContIxT $ \k -> k a

bin (ContIxT m) f = ContIxT $ \k -> m $ \a -> unContT (f a) k
