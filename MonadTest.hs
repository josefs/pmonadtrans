{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module MonadTest where

{- This is an idea due to Conor. Though he doesn't provide any
   concrete examples of higher kinded versions of this sort of monad.

   It's also too bad that it's a multiparameter type class without
   any functional dependencies
-}

{-
class MonadM m (~>) where
  returnm :: a ~> m a
  bindm :: (a ~> m b) -> (m a ~> m b)
-}

class MonadM m arr where
  returnm :: arr a (m a)
  bindm :: arr a (m b) -> (arr (m a) (m b))

data Id a = Id { unId :: a }

instance MonadM Id (->) where
  returnm a = Id a
  bindm f (Id a) = Id $ unId (f a)

{- An experiment using an associated type to help inference.
   The monad decides the kind of the arrow.
-}

class MonadM' (m :: k -> k) where
  type Arr m :: k -> k -> *
  returnm' :: (Arr m) a (m a)
  bindm' :: (Arr m) a (m b) -> ((Arr m) (m a) (m b))

instance MonadM' Id where
  type Arr Id = (->)
  returnm' = Id
  bindm' f (Id a) = Id $ unId (f a)

data IdT m a = IdT { unIdT :: m a }

instance Monad m => MonadM' (IdT m) where
  type Arr (IdT m) = (->)
  returnm' a = IdT (return a)
  bindm' f (IdT m) = IdT $ m >>= (unIdT . f)

{- Asch. The type operator :-> needs to be fully applied.
   Turning it into a newtype kind of loses the point of the
   whole excercise. I'd like to use bindm' seamlessly without having
   to wrap and unwrap newtype wrappers.

type f :-> g = forall a . f a -> g a

instance MonadM' IdT where
  type Arr IdT = (:->)
-}

newtype f :-> g = ColonArrow (forall a. f a -> g a)

instance MonadM' IdT where
  type Arr IdT = (:->)
  returnm' = ColonArrow $ \a -> IdT a
  bindm' (ColonArrow f) = ColonArrow $ \ma -> f (unIdT ma)
