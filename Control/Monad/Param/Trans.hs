{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Control.Monad.Param.Trans where

import Control.Monad.Param

class PMonadTrans t where
  plift :: PMonad m => m i j a -> t m '(s,i) '(s,j) a
