{-# LANGUAGE PolyKinds #-}
module Control.Monad.Indexed where

class IxFunctor f where
  ifmap :: (a -> b) -> f i j a -> f i j b

class IxMonad m where
  ireturn :: a -> m i i a
  ibind :: m i j a -> (a -> m j k b) -> m i k b

ijoin :: IxMonad m => m i j (m j k a) -> m i k a
ijoin mm = mm `ibind` \m -> m

class IxMonadPlus m where
  izero :: m i i a
  iplus :: m i j a -> m i j a -> m i j a

class IxApplicative m where
  ipure :: a -> m i i a
  iapp :: m i j (a -> b) -> m j k a -> m i j b

class IxMonad m => IxMonadFix m where
  ifix :: (a -> m i j a) -> m i j a
