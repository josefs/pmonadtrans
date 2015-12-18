{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Control.Monad.Indexed.Trans where

import Control.Monad.Indexed

class IxMonadTrans t where
  ilift :: IxMonad m => m i j a -> t m '(s,i) '(s,j) a
