{-# LANGUAGE
		NoImplicitPrelude,
		GADTs,
		PolyKinds,
		TypeFamilies,
		TypeOperators,
		ScopedTypeVariables
  #-}

{-|
Module          : Abstract.Category
Description     : Class of Categories
Copyright       : (c) Nicolas Godbout, 2015
License         : BSD-3
Maintainer      : nicolas.godbout@gmail.com

This module is the foundation of the alt-base package and is imported by
most other modules.
-}
module Abstract.Category (
		-- * Category class
		Category(..),
		-- * Type-level (large) categories.
		id, (.),
		-- * Term-level (small) categories.
		(:=:)(..), eqTerm
	) where

-- alt-base modules
import Abstract.Reflection
import Object.Identity

-- base modules
import Data.Bool (Bool(..))
import Data.Eq (Eq(..))
import Data.Maybe (Maybe(..))
import Data.Typeable


{- | Class of Categories.

Instances must satisfy the following laws:

prop> f . (idC (source f)) == f
prop> (idC (target g)) . g == g
prop> h . (g . f) == (h . g) . f
prop> g . f == bottom "if and only if" idS g `idEq` idT f

-}
class Category hom where
	type IfC hom a b :: *
	type Obj hom a   :: *

	-- | Get the source of the morphism.
	source 	:: hom a b -> Obj hom a

	-- | Get the target of the morphism.
	target  :: hom a b -> Obj hom b

	-- | Build an identity morphism over the given object.
	idC		:: Obj hom a -> hom a a

	-- | Composition with a witness.
	dotW 	:: IfC hom b b'
			-> hom b' c -> hom a b
			-> hom a c

id :: (Category c, Obj c a ~ Proxy a) => c a a
{-# INLINE id #-}
id = idC Proxy

(.) :: (Category hom, IfC hom b b ~ (b :~: b))
    => hom b c -> hom a b
    -> hom a c
{-# INLINE (.) #-}
g . f = dotW Refl g f


instance Category (->) where
	type IfC (->) a b = a :~: b
	type Obj (->) a   = Proxy a

	source (f :: a -> b) = Proxy :: Proxy a
	target (f :: a -> b) = Proxy :: Proxy b

	idC Proxy = \x -> x

	{-# INLINE dotW #-}
	dotW Refl = \g f -> \x -> g (f x)

instance Category (,) where
	type IfC (,) a b = a :=: b
	type Obj (,) a   = a

	source (x,_) = x
	target (_,y) = y

	idC x = (x,x)

	{-# INLINE dotW #-}
	dotW (Equal _) = \(_,y) (x,_) -> (x,y)



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
