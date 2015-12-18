{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
module Control.Monad.Indexed.Trans.TF where

type family Fst (p::(k,k')) :: k where
  Fst '(k,k') = k
type family Snd (p::(k',k)) :: k where
  Snd '(k',k) = k
