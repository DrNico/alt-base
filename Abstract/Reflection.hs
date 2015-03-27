{-# LANGUAGE
    GADTs, PolyKinds, TypeOperators
  #-}

module Abstract.Reflection (
        Refl(..)
    ) where

-- alt-base modules
import Object.Identity

-- base modules
import Data.Proxy
import Data.Type.Coercion
import Data.Type.Equality

{- | Reflection of equality of the contents of a type function, both at the type 
and the term levels.
-}
class Refl t where
    refl    :: Eq b => t a -> t b -> Maybe (a :~: b)
    -- 't' is not poly-kinded since its contents is an instance of Eq...

-----
-- Instances for 'base' types
-----

instance Refl Proxy where
    refl Proxy Proxy = go undefined
        where
            go :: a :~: b -> Maybe (a :~: b)
            go Refl = Just Refl

instance Refl Id where
    refl (Id x) (Id y) = go undefined x y
        where
            go :: Eq b => a :~: b -> a -> b -> Maybe (a :~: b)
            go Refl = \x y -> if x == y then Just Refl else Nothing

