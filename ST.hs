{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.Indexed.ST where

import Control.Monad.Indexed

{- Use an explicit type environment to implement polymorphic references
   a la ST monad. -}

data ST i j a where
  ST :: i -> (a,j) -> ST i j a

data Env :: [*] -> * where
  Emp :: Env '[]
  Ext :: t -> Env e -> Env (t ': e)

data Var :: [k] -> k -> * where
  Zro :: Var (t ': r) t
  Suc :: Var r tp -> Var (t ': r) tp

lkup :: Var e t -> Env e -> t
lkup Zro (Ext t _) = t
lkup (Suc v) (Ext _ e) = lkup v e

lkup' :: SubEnv e e' => Var e t -> Env e' -> t
lkup' v e = lkup v (stripEnv e)

update :: t -> Var e t -> Env e -> Env e
update t Zro (Ext _ e) = Ext t e
update t (Suc v) (Ext r e) = Ext r (update t v e)

update' :: SubEnv e e' => t -> Var e t -> Env e' -> Env e'
update' = updateEnv

--newD :: t -> (Env       e,  Var       (s ': e) s)
--          -> (Env (t ': e), Var (t ': (s ': e)) t)
--newD val (e,v) = (Ext val e, Suc v)

new :: t -> Env e -> (Env (t ': e), Var (t ': e) t)
--new t e = (Ext t e, newVar t e)
new = undefined

class SubEnv e e' where
  stripEnv :: Env e' -> Env e
  updateEnv :: t -> Var e t -> Env e' -> Env e'

instance SubEnv e e where
  stripEnv e = e
  updateEnv = update

instance SubEnv e e' => SubEnv e (t ': e') where
  stripEnv (Ext _ e) = stripEnv e
  updateEnv t v (Ext m e) = Ext m (updateEnv t v e) 
{-
class NewVar t t' where
  newVar :: Env e -> Var (t' ': e) t

instance NewVar t t where
  newVar e = 

instance NewVar (t ': e) t where
  newVar 
-}
--newVar = undefined

{-

let emp = Emp
    (e1,v1) = new 'a' emp -- e1 :: Env [Char], v1 :: Var [Char] Char
    (e2,v2) = new 'b' e1  -- e2 :: Env [Char,Char], v2 :: Var [Char,Char] Char
    'a' == lkup v1 e2

-}

--newVar :: t -> Env e -> Var (t ': e) t
--newVar _ Emp = Zro
--newVar t (Ext _ e) = Suc (newVar t e)

--newVar :: Env e -> Var (t ': e) t'
--newVar Emp = Zro
--newVar (Ext _ e) = Suc (newVar e)
{-
data Ref r a

newRef :: ST i (r:i) r

getRef :: 

setRef

runST :: ST () e a -> a
-}
