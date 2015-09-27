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

import Alt.Object.Identity

-- base modules
import Data.Bool (Bool(..))
import Data.Eq (Eq(..))
import Data.Maybe (Maybe(..))
import Data.Typeable (Typeable, Proxy(..), (:~:)(..), eqT)

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
    type ObjEq eq a :: *
    type ObjEq eq a = a

    -- ## remove Typeable constraint, have it live in the Object instead
    witness :: (a' ~ ObjEq eq a, b' ~ ObjEq eq b, Typeable a, Typeable b)
            => a' -> b' -> Maybe (eq a b)

    reflect :: eq a b -> eq b a


{- | Type equivalence. Objects @a@ and @b@ are deemed equivalent if they
satisfy the constraint @a ~ b@.
'Proxy' can be fed as arguments to 'witness'.
-}
instance Equivalence (:~:) where
    type ObjEq (:~:) a = Proxy a

    witness (x :: Proxy a) (y :: Proxy b) = eqT

    reflect Refl = Refl

{- | Term equality.

Term equality is inhabited if both the types and the values of given arguments
are equal.
-}
data a :=: b where
    Equal   :: (Eq a)
            => a
            -> a :=: a

instance Equivalence (:=:) where
    type ObjEq (:=:) a = Id a

    witness (Id (x :: a)) (Id (y :: b)) =
        case eqT :: Maybe (a :~: b) of
            Just Refl ->
                case x == y of
                    True -> Just (Equal x)
                    False -> Nothing

    reflect (Equal a) = Equal a

