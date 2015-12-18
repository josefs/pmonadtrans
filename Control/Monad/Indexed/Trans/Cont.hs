{-# LANGUAGE GADTs #-}
module Control.Monad.Indexed.Trans.Cont where

-- I don't think it's possible to define a sensible version
-- of the continuation monad transformer. It's just not clear
-- that we have enough type parameters for the two occurrences
-- of the monad being transformed.

import Control.Monad.Indexed
import Control.Monad.Indexed.Trans.TF

data ContIxT m i j a where
  ContIxT :: { unContT :: ((a -> m (Snd i) (Snd i) (Fst i)) -> m (Snd j) (Snd j) (Fst j)) } -> ContIxT m i j a

instance IxMonad m => IxMonad (ContIxT m) where
  ireturn a = ContIxT $ \k -> k a
  ContIxT m `ibind` f = ContIxT $ \k -> m $ \a -> unContT (f a) k
