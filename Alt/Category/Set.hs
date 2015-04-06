{-# LANGUAGE
		NoImplicitPrelude,
		GADTs,
		TypeFamilies,
		TypeOperators,
		StandaloneDeriving,
		ScopedTypeVariables
	#-}

{- |
Module          : Alt.Category.Set
Description     : Category __Set__
Copyright       : (c) Nicolas Godbout, 2015
License         : BSD-3
Maintainer      : nicolas.godbout@gmail.com

This module provides an implementation of the category __Set__ having small sets
as objects and functions (defined as relations) as arrows.
-}
module Alt.Category.Set (
		CatSet,
		ObjSet,
		EqSet,
		fromList
	) where

-- alt-base modules
import Alt.Abstract.Category
import Alt.Abstract.Equivalence
import Alt.Transform.Swap

-- base modules
import Control.Monad (Functor(..), Monad(..))
import Data.Eq (Eq)
import Data.Int (Int)
import Data.List (map, zipWith, iterate, (!!), elemIndex)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Typeable (Typeable, Proxy(..), eqT)

-- alt-base prelude
import Alt.Prelude

{- | The category __Set__, wherein objects are small sets and arrows are 
functions, encoded as relations between sets.

The members of the small sets must at least have an Equality relation for
the sets themselves to have one.
-}
data CatSet a b where
	FuncSet		:: (Eq a, Eq b, Typeable a, Typeable b) => {
					domSet		:: [a],
					arrSet 		:: [Int],
					codSet      :: [b]
				} -> CatSet a b

{- | Object of the category __Set__.
-}
data ObjSet a where
	ObjSet		:: (Eq a, Typeable a) => [a] -> ObjSet a

{- | Equality witness of objects of the category __Set__.
-}
data EqSet a b where
	EqSet		:: [Int]
				-> EqSet a b

{- | Build an arrow of the category __Set__ from a list of source-target
tuples.
-}
fromList :: (Eq a, Eq b, Typeable a, Typeable b)
         => [(a,b)] -> CatSet a b
fromList lst =
	FuncSet
		(map fst lst)
		(zipWith (\_ i -> i) lst (iterate (+ 1) 0))
		(map snd lst)

-----
-- Instances
-----

-- instance Equivalence EqSet where
witness' :: (Eq a, Eq b, Typeable a, Typeable b)
		 => ObjSet a -> ObjSet b
	     -> Maybe (EqSet a b)
witness' ((ObjSet as) :: ObjSet a) ((ObjSet bs) :: ObjSet b) = do
		refl <- eqT :: Maybe (a :~: b)
		quiver refl as bs
		where
		quiver :: (a :~: b) -> [a] -> [b] -> Maybe (EqSet a b)
		quiver Refl as bs = do
			perm <- swap $ map (\a -> elemIndex a bs) as
			return $ EqSet perm
		-- ## check that this is a bijection

instance Equivalence EqSet where
	type EqO EqSet a = ObjSet a

	witness = witness'

instance Category CatSet where
	type EqC CatSet a b = EqSet (ObjSet a) (ObjSet b)
	type Obj CatSet a   = ObjSet a

	source (FuncSet s _ _) = ObjSet s
	target (FuncSet _ _ t) = ObjSet t

	idC (ObjSet o) = FuncSet {
		domSet = o,
		arrSet = zipWith (\_ i -> i) o (iterate (+ 1) 0),
		codSet = o
	}

	dotW refl = dot
		where
			dot (FuncSet sg g tg) (FuncSet sf f tf) =
				FuncSet sf (map ((!!) g) f) tg


