module Control.Monad.Param.Do where

import Control.Monad.Param

{- These definitions allows for using do-notation when programming
   with indexed monads, by using the RebindableSyntax language pragma.
-}

return :: PMonad m => a -> m i i a
return = ireturn

(>>=) :: PMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) a b = ibind a b
