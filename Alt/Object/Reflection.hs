{-# LANGUAGE
        GADTs,
        TypeOperators,
        ScopedTypeVariables
  #-}

module Alt.Object.Reflection (
        (:=:)(..), eqTerm
    ) where

import Data.Typeable (Typeable, (:~:)(..), eqT)

{- | Term equality.

Term equality is inhabited if both the types and the values of given arguments
are equal.
-}
data a :=: b where
    Equal   :: a -> a :=: a

eqTerm :: (Typeable a, Typeable b, Eq a) => a -> b -> Maybe (a :=: b)
eqTerm (x :: a) (y :: b) =
    case eqT :: Maybe (a :~: b) of
        Just Refl ->
            case x == y of
                True -> Just (Equal x)
                False -> Nothing
