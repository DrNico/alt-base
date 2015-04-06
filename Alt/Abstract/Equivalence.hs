{-# LANGUAGE
        NoImplicitPrelude,
        GADTs,
        PolyKinds,
        TypeFamilies,
        TypeOperators,
        ScopedTypeVariables
    #-}

module Alt.Abstract.Equivalence (
        Equivalence(..),
        (:~:)(..), -- re-export
        (:=:)(..)
    ) where

import Data.Bool (Bool(..))
import Data.Eq (Eq(..))
import Data.Maybe (Maybe(..))
import Data.Typeable (Typeable, (:~:)(..), eqT)

infixr 4 :=:

{- | Equivalence class.

Instances must satisfy the laws:

  1. reflectivity

     prop> witness x y == fmap reflect (witness y x)

  2. involution of reflectivity

     prop> reflect . reflect == id

  3. transitivity

     prop> witness x y >> witness y z >> return () == witness x z >> return ()
-}
class Equivalence eq where
    type EqO eq a :: *
    type EqO eq a = a

    witness :: (a' ~ EqO eq a, b' ~ EqO eq b, Typeable a, Typeable b, Eq a, Eq b)
            => a' -> b' -> Maybe (eq a b)

    reflect :: eq a b -> eq b a


{- | Type equivalence. Objects @a@ and @b@ are deemed equivalent if they
satisfy the constraint @a ~ b@.
'Proxy' can be fed as arguments to 'witness'.
-}
instance Equivalence (:~:) where
    witness (x :: a) (y :: b) = eqT

    reflect Refl = Refl

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


instance Equivalence (:=:) where
    witness = eqTerm

    reflect (Equal a) = Equal a

