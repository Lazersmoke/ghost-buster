{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
module Data.SuchThat
  (SuchThat(..)
  ,SuchThatStar(..)
  ,ForAny
  ,Some
  ,ambiguate
  ,ambiguously
  ) where

import GHC.Exts (Constraint)

data SuchThat (c :: [k -> Constraint]) (p :: k -> *) = forall s. Constrain c s => SuchThat (p s)
-- This should be `type SuchThatStar c = SuchThat c Id`, but Id is partially applied then
data SuchThatStar (c :: [* -> Constraint]) = forall s. Constrain c s => SuchThatStar s
--type SuchThat (c :: [k -> Constraint]) (p :: k -> *) = forall s. Constrain c s => p s

type family Constrain (c :: [k -> Constraint]) (x :: k) :: Constraint where
  Constrain (c ': '[]) x = c x
  Constrain (c ': cs) x = (c x,Constrain cs x)
  Constrain '[] x = x ~ x

-- An unconstrained SuchThat. Only useful for Ghost Busting (removing phantom types) and reading structure
--data ForAny (p :: k -> *) = forall s. ForAny (p s)
type ForAny = SuchThat '[]

type Some x = SuchThat '[x]

-- "Inject" a value into a SuchThat. It becomes more ambiguous because it throws out the exact type of s
ambiguate :: Constrain c s => p s -> SuchThat c p
ambiguate = SuchThat

ambiguously :: (forall s. Constrain c s => p s -> r) -> SuchThat c p -> r
ambiguously f (SuchThat ps) = f ps

-- Escape from a SuchThat by running a continuation with the context
--onFA :: SuchThat c p -> (forall s. Constrain c s => p s -> r) -> r
--onFA (SuchThat ps) f = f ps
