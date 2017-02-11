{-|
Module      : Control.Monad.Param.Do

These definitions allows for using do-notation when programming with
parameterized monads, by using the RebindableSyntax language pragma.
-}
module Control.Monad.Param.Do where

import Control.Monad.Param

return :: PMonad m => a -> m i i a
return = preturn

(>>=) :: PMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) a b = pbind a b
