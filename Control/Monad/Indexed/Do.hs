module Control.Monad.Indexed.Do where

import Control.Monad.Indexed

{- These definitions allows for using do-notation when programming
   with indexed monads, by using the RebindableSyntax language pragma.
-}


return :: IxMonad m => a -> m i i a
return = ireturn

(>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) a b = ibind a b
