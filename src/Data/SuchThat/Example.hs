{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Data.SuchThat.Example where

import Data.SuchThat

-- Requiring typeclass adherence, but allowing mixed lists with single parameter SuchThat
-- Normal existential stuff
type ShowBox = SuchThat '[Show] []

-- This works because `Show a => [a]` has a Show instance of its own
showShowBox :: ShowBox -> String
showShowBox = ambiguously show

-- Forgeting spooky Phantom Types using ForAny
-- Flag is True if the string has been properly sanitized
data Safe (sanitzied :: Bool) where
  Safe :: String -> Safe 'True
  Unsafe :: String -> Safe 'False

-- "Forget" whether it is sanitized or not
type NoSpookyGhosts = ForAny Safe

-- List of both safe and unsafe strings. Thank goodness we got rid of those gosh darn ghosts!
listOfStrings :: [NoSpookyGhosts]
listOfStrings = [ambiguate notSafeString,ambiguate safeString]

notSafeString :: Safe 'False
notSafeString = Unsafe "meow"

safeString :: Safe 'True
safeString = Safe "asdf"
