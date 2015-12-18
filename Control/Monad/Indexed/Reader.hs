{-# LANGUAGE GADTs #-}
{- Experiment to see if we can actually implement an indexed reader
monad in a meaningful way. It doesn't seem to make any sense.

It would fit into the framework of Dominic Orchard with a trivial
unit and plus operation. Or would it? Is there a way to enforce that
the two computations in bind have the same reader? For that we would
need some kind of constraint which I don't think his framework allows
for.
-}
module Control.Monad.Indexed.Reader where

import Control.Monad.Indexed

data ReaderIx r s a where
  ReaderIx :: { runReaderIx :: r -> a } -> ReaderIx r s a

instance IxMonad ReaderIx where
  ireturn a = ReaderIx $ \_ -> a
  ReaderIx f `ibind` m = ReaderIx $ \r -> runReaderIx (m (f r)) r
