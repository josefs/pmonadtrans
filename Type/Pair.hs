{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
module Type.Pair where

type family Fst (p::(k,k')) :: k where
  Fst '(k,k') = k
type family Snd (p::(k',k)) :: k where
  Snd '(k',k) = k
