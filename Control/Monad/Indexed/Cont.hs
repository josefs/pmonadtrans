{-# LANGUAGE GADTs #-}
module Control.Monad.Indexed.Cont where

import Control.Monad.Indexed

data ContIx r o a where
  ContIx :: { unCont :: (a -> o) -> r } -> ContIx r o a

instance IxMonad ContIx where
  ireturn a = ContIx $ \k -> k a
  ContIx f `ibind` g = ContIx $ \k -> f $ \a -> unCont (g a) k
