{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
-- | Provides the type @SuchThat@, which encapsulates an existentially quantified type.
module Data.SuchThat
  (SuchThat(..)
  ,ambiguate
  ,ambiguously
  ,ForAny
  ,discoverStructure
  ,naturalTransformation
  ,unNaturalTransformation
  ,Some
  ,some
  ,Satisfies
  ,Constrain
  ) where

import Data.Functor.Identity
import GHC.Exts (Constraint)
import Control.Monad (void)

-- Ideally, we would write:
--
--   type SuchThat (c :: [k -> Constraint]) (p :: k -> *) = forall s. Constrain c s => p s
-- 
-- But type synonyms don't work that way

-- | A @'SuchThat' c p@ is a @p s@, where @s@ is such that it satisfies all of the constraints in @c@.
-- Note that you must always wrap results from pattern-matching on the @'SuchThat'@ constructor in a @'SuchThat'@, or GHC will complain about @s@ escaping its scope.
data SuchThat (c :: [k -> Constraint]) (p :: k -> *) = forall s. Constrain c s => SuchThat (p s)

-- | Inject a value into a @'SuchThat'@. It becomes more ambiguous because it throws out the exact type of @s@.
ambiguate :: Constrain c s => p s -> SuchThat c p
ambiguate = SuchThat

-- | Escape from a @'SuchThat'@ by running a universally quantified continuation.
ambiguously :: (forall s. Constrain c s => p s -> r) -> SuchThat c p -> r
ambiguously f (SuchThat ps) = f ps

-- | A @'Some' c@ is some value that satisfies all the constraints in @c@.
type Some c = SuchThat c Identity

-- | Build a @'Some' c@ from some thing that satisfies @c@.
some :: Constrain c a => a -> Some c
some = ambiguate . Identity

-- | A @'ForAny' f@ is an @f@ containing some arbitrary values, meaning you can only apply operations that would work /for any/ type of value it contains.
-- This is useful if the type parameter is phantom because it allows you to ignore it. 
-- This is called "ghost busting" because it removes the phantom.
-- It can also be used for reading only the /structure/ of something rather than the actual values.
-- For example, @'ForAny' []@ is isomorphic to both the natural numbers, and the fixed point of @'Data.Maybe.Maybe'@.
type ForAny = SuchThat '[]

-- | If the type constructor in question is a functor, we can scope out its structure by converting every element to @()@.
-- Note that this is actually the most general way to do this, since the type signature would otherwise be @Functor p => SuchThat c p -> (forall x. x -> a) -> p a@.
-- That means the output type must be isomorphic to @()@ because that is the terminal object in @Hask@.
--
-- @
--   discoverStructure (ambiguate ['a','b','c'] :: ForAny []) = [(),(),()]
--   discoverStructure (ambiguate (Just "hi") :: ForAny Maybe) = Just ()
--   discoverStructure (ambiguate Nothing :: ForAny Maybe) = Nothing
-- @
discoverStructure :: Functor p => SuchThat c p -> p ()
discoverStructure = ambiguously void

-- | A function between @'SuchThat' c@'s between different type constructors is isomorphic to a natural transformation between those type constructors.
-- This is especially useful if @c ~ '[]@, meaning you have a @'ForAny'@.
naturalTransformation :: (forall x. f x -> g x) -> SuchThat c f -> SuchThat c g
naturalTransformation n = ambiguously (ambiguate . n)

-- Type class machinery to make the isomorphism work. This is needed because we can't partially apply the ~
-- It's also a hack because GHC is pretty bad at inferring the right things here.
class a ~ b => Is a b where {}
instance Is a a where {}

-- | The other half of the aforementioned isomorphism.
unNaturalTransformation :: forall f g x. (forall c. SuchThat c f -> SuchThat c g) -> (f x -> g x)
unNaturalTransformation u fx = ambiguously id $ u (ambiguate fx :: SuchThat '[Is x] f)

-- | A @'Satisfies' x@ is a value that satisfies the single constraint x.
type Satisfies x = Some '[x]

-- | A type level fold for applying many constraints to the same thing
type family Constrain (c :: [k -> Constraint]) (x :: k) :: Constraint where
  Constrain (c ': '[]) x = c x
  Constrain (c ': cs) x = (c x,Constrain cs x)
  Constrain '[] x = x ~ x
