{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE RankNTypes           #-}
module Control.Monad.Param.P where

import Type.Pair

import Control.Monad.Param
import Control.Monad.Param.Trans
import Control.Monad.Trans.Class

-- | Takes a regular monad and makes it into an indexed monad.
data P m i j a where
  P :: { unP :: m a } -> P m i j a

instance Functor f => PFunctor (P f) where
  pfmap f (P m) = P $ fmap f m

instance Applicative m => PApplicative (P m) where
  ppure a = P $ pure a
  P m `papp` P n = P $ m <*> n

instance Monad m => PMonad (P m) where
  preturn a = P (return a)
  pbind (P m) f = P $ m >>= unP . f 

runP :: P m i j a -> m a
runP = unP

{- Possible but requires dodgy FlexibleInstances
instance Monad m => Monad (P m i i) where
  return = preturn
  (>>=) = pbind
-}

{- This type cannot be an instance of MonadTrans because the monad argument
   is in the wrong position. Though morally it really is a MonadTrans.

   If MonadTrans was defined differently it would be possible, though.

   class MonadTrans t where
     type Mon t :: * -> *
     lift :: Mon t a -> t a
-}

{- This doesn't work because a monad transformer cannot deal with an underlying
   monad that changes types. We would need to require something stronger,
   like a MonadLayer perhaps.

-- | Takes a monad transformer and makes it into an indexed monad transformer
data PT t (m :: k -> k -> * -> *) i j a where
  PT :: { unPT :: t (m (Snd i) (Snd j)) a } -> PT t m i j a

instance (PMonad m {- , Monad (m (Snd i) (Snd i)) -} ) => PMonad (PT t m) where
--  preturn a = PT (lift (preturn a))
  (PT m) `pbind` f = PT $ m >>= unPT . f
-}

-- | Takes an indexed monad and turns it onto a regular monad.
--   It is somewhat limited in the sense that the type indexes need to be
--   the same.
data PM m i a where
  PM :: { unPM :: m i i a } -> PM m i a

instance PFunctor m => Functor (PM m i) where
  fmap f (PM m) = PM $ pfmap f m

instance (PMonad m, PFunctor m) => Monad (PM m i) where
  return a = PM (preturn a)
  PM m >>= f = PM $ m `pbind` (unPM . f)

instance (PMonad m, PFunctor m) => Applicative (PM m i) where
  pure = return
  m <*> n = m >>= \f -> n >>= \a -> return (f a)

promote :: m i i a -> PM m i a
promote = PM

runPM :: PM m i a -> m i i a
runPM = unPM

-- | Takes an indexed monad transformer and creates a regular monad transformer
data PT t s i m a where
  PT :: { unPT :: t (P m) '(s,i) '(s,i) a } -> PT t s i m a

instance PFunctor (t (P m)) => Functor (PT t s i m) where
  fmap f (PT t) = PT $ pfmap f t
instance (PMonad (t (P m)), PFunctor (t (P m))) => Applicative (PT t s i m) where
  pure = return
  m <*> n = m >>= \f -> n >>= \a -> return (f a)

instance (PMonad (t (P m)), PFunctor (t (P m))) => Monad (PT t s i m) where
  return a = PT $ preturn a
  PT m >>= f = PT $ m `pbind` \a -> unPT (f a)

instance PMonadTrans t => MonadTrans (PT t s i) where
  lift m = PT $ plift $ P m

runPT :: PT t s i m a -> (forall mi . t mi '(s,i) '(s,i) a -> mi i i a) -> m a
runPT (PT t) run = runP (run t)

-- A test

class MonadTrans' t where
  type Mon t :: * -> *
  liftT :: Mon t a -> t a

instance Monad m => MonadTrans' (P m i j) where
  type Mon (P m i j) = m
  liftT = P

{- Hrmph, this doesn't seem to work out either.

data PMT t (m :: k -> k -> * -> *) i j a where
  PMT :: { unPMT :: t (m (Snd i) (Snd j)) a } -> PMT t m i j a

instance (MonadTrans' (t (m i j)), Mon (t (m i j)) ~ m i j) => PMonad (PMT t m) where
  preturn a = PMT (liftT (preturn a))
--  (PMT m) `pbind` f = PMT $ m >>= unPMT . f

-}

{- This doesn't work because PM needs both parameters to be the same

-- | Takes a regular monad transformer and produces an indexed transformer
data PMT t m i j a where
  PMT :: t (PM m (Fst i)) a -> PMT t m i i a

--instance PMonadTrans (PMT t) where
--  plift m = PMT $ lift (PM m)
-}
