{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
-- Note the `forall`s in this module are superfluous unless otherwise specified
module Data.SuchThat.Example where

import Data.SuchThat
import Data.Foldable (toList)

demonstrate :: IO ()
demonstrate = do
  putStrLn $ "[stringBox,intBox] is " ++ show (map (ambiguously show) [stringBox,intBox])
  putStrLn $ "notSafeString is " ++ show notSafeString
  putStrLn $ "safeString is " ++ show safeString
  -- Ambiguously show every string in the list, then print that list
  putStrLn $ "listOfStrings is " ++ show (map (ambiguously show) listOfStrings)
  putStrLn $ "listOfStrings' is " ++ show (map (ambiguously show) listOfStrings')
  putStrLn $ "listOfStrings'' is " ++ show listOfStrings''
  putStrLn $ "(discoverStructure someMaybe) is " ++ show (discoverStructure someMaybe)
  putStrLn $ "(discoverStructure someMaybeAsEitherUnit) is " ++ show (discoverStructure someMaybeAsEitherUnit)
  putStrLn $ "(discoverStructure someMaybeAsList) is " ++ show (discoverStructure someMaybeAsList)

-- A list that is all of one particular Show-able type, but we don't know which one.
type ShowBox = SuchThat '[Show] []

-- Observe how both lists have the same type, but contain different types of items
stringBox :: ShowBox
stringBox = ambiguate ["meow","123","asdf"]

intBox :: ShowBox
intBox = ambiguate [(1 :: Int),2,3] -- Force it to be `Int` with a type signature

-- This works because `Show a => [a]` has a Show instance of its own
showShowBox :: ShowBox -> String
showShowBox = ambiguously show

-- Forgeting spooky Phantom Types using ForAny
-- Flag is True if the string has been properly sanitized
data SafeString (sanitzied :: Bool) where
  Safe :: String -> SafeString 'True
  Unsafe :: String -> SafeString 'False

instance Show (SafeString b) where
  show (Safe s) = "<The safe string: `" ++ s ++ "`>"
  show (Unsafe s) = "<The unsafe string: `" ++ s ++ "`>"

-- This is a perfectly normal polymorphic function on a perfectly normal GADT:
overString :: forall b. (String -> String) -> SafeString b -> SafeString b
overString f (Safe s) = Safe (f s)
overString f (Unsafe s) = Unsafe (f s)

-- As is this one:
getString :: forall b. SafeString b -> String
getString (Safe s) = s
getString (Unsafe s) = s

-- Forget whether it is safe or not by busting the phantom type.
type PotentiallyUnsafeString = ForAny SafeString

-- This is user input and hasn't been sanitized yet
notSafeString :: SafeString 'False
notSafeString = Unsafe "meow"

-- This is a statically known string that we have manually vetted as safe
safeString :: SafeString 'True
safeString = Safe "asdf"

-- List of both safe and unsafe strings. Thank goodness we got rid of those gosh darn ghosts!
listOfStrings :: [PotentiallyUnsafeString]
listOfStrings = [ambiguate notSafeString,ambiguate safeString]

-- We can update each string by using `ambiguously`. We use ambiguate to re-ambiguate each string so they can still be stored in the list
listOfStrings' :: [PotentiallyUnsafeString]
listOfStrings' = map (ambiguously $ ambiguate . overString (++"!!!")) listOfStrings

-- We can also escape the SuchThat entirely by using a continuation that unifies the types
-- `getString` maps both kinds of SafeString to the same type (String), so it can be used to remove the polymorphism
listOfStrings'' :: [String]
listOfStrings'' = map (ambiguously getString) listOfStrings'

-- Here is a natural isomorphism between `Either () x` and `Maybe x`, since both of them are just `x + 1`
eitherUnitToMaybe :: forall x. Either () x -> Maybe x
eitherUnitToMaybe (Left ()) = Nothing
eitherUnitToMaybe (Right x) = Just x

maybeToEitherUnit :: forall x. Maybe x -> Either () x
maybeToEitherUnit Nothing = Left ()
maybeToEitherUnit (Just x) = Right x

-- Now let's say we have:
someMaybe :: ForAny Maybe
someMaybe = ambiguate $ Just (error "Arbitrary value that won't be inspected")

-- We can convert it to `Either ()` with a natural transformation
someMaybeAsEitherUnit :: ForAny (Either ())
someMaybeAsEitherUnit = naturalTransformation maybeToEitherUnit someMaybe

-- `toList :: Maybe a -> [a]` is also a natural transformation
someMaybeAsList :: ForAny []
someMaybeAsList = naturalTransformation toList someMaybe
