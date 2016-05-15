{-# LANGUAGE PolyKinds #-}
module Control.Monad.Param where

class PFunctor f where
  pfmap :: (a -> b) -> f i j a -> f i j b

class PApplicative m => PMonad m where
  preturn :: a -> m i i a
  pbind :: m i j a -> (a -> m j k b) -> m i k b

pjoin :: PMonad m => m i j (m j k a) -> m i k a
pjoin mm = mm `pbind` \m -> m

class PMonad m => PMonadPlus m where
  pzero :: m i i a
  pplus :: m i j a -> m i j a -> m i j a

class PFunctor m => PApplicative m where
  ppure :: a -> m i i a
  papp :: m i j (a -> b) -> m j k a -> m i k b

class PMonad m => PMonadFix m where
  pfix :: (a -> m i i a) -> m i i a
