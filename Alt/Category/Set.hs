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
        -- * Data types
        CatSet,
        ObjSet,
        EqSet,
        -- * Special objects
        initial,
        terminal,
        -- * Building __Set__ arrows
        zero,
        one,
        fromList,
        -- * Queries
        null,
        elem
    ) where

-- alt-base modules
import Alt.Abstract.Category
import Alt.Abstract.Equivalence
import Alt.Transform.Swap

-- base modules
import Control.Monad (Functor(..), Monad(..))
import Data.Bool (Bool(..), (||))
import Data.Eq (Eq)
import Data.Int (Int)
import Data.List (map, zipWith, iterate, (!!), elemIndex)
import qualified Data.List (elem)
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
    FuncSet     :: (Eq a, Eq b, Typeable a, Typeable b) => {
                    domSet      :: [a],
                    arrSet      :: [Int],
                    codSet      :: [b]
                } -> CatSet a b

{- | Object of the category __Set__.
-}
data ObjSet a where
    ObjSet      :: (Eq a, Typeable a) => [a] -> ObjSet a

{- | Equality witness of objects of the category __Set__.
-}
data EqSet a b where
    EqSet       :: [Int]
                -> EqSet a b

{- | The initial object of __Set__, the empty set.
-}
initial :: (Eq a, Typeable a)
        => ObjSet a
initial = ObjSet []

{- | The Zero arrow of __Set__, with source and target its initial object, /i.e./ the
    empty set.
-}
zero :: (Eq a, Eq b, Typeable a, Typeable b)
     => CatSet a b
zero = FuncSet [] [] []

{- | The One arrow of __Set__, with source and target its terminal object, /i.e./ 
    the singleton @{()}@.
-}
one :: CatSet () ()
one = FuncSet [()] [0] [()]

{- | Build an arrow of the category __Set__ from a list of source-target
tuples.

The objects of the sets themselves are required to be instances of 'Eq'.
-}
fromList :: (Eq a, Eq b, Typeable a, Typeable b)
         => [(a,b)] -> Maybe (CatSet a b)
fromList lst =
    if hasDup (map fst lst)
    then Nothing
    else Just $ FuncSet
        (map fst lst)
        (zipWith (\_ i -> i) lst (iterate (+ 1) 0))
        (map snd lst)
    where
    hasDup :: Eq a => [a] -> Bool
    hasDup [] = False
    hasDup (x:xs) = (Data.List.elem x xs) || hasDup xs

{- | Returns 'True' if the argument is a zero arrow.
-}
null :: CatSet a b -> Bool
null (FuncSet [] [] _)  = True
null _                  = False

{- | Test if the arrow has the given object as valid target.

Since the domain of the arrow is a singleton, its image is also a singleton.
The arrow therefore represents a single object, tested to be an element of the
given set (an object of __Set__).
-}
elem :: CatSet (Terminal CatSet) b -> ObjSet b -> Bool
elem (FuncSet [()] [j] ts) (ObjSet os) =
    Data.List.elem (ts !! j) os

-----
-- Instances
-----

class Category cat => HasTerminal cat where
    type Terminal cat :: *

    terminal :: Obj cat (Terminal cat)

instance HasTerminal CatSet where
    type Terminal CatSet = ()

{-  The terminal object of __Set__, a singleton. In this implementation, there
is a terminal object for each Haskell type. The terminal object is therefore not
technically unique. However, a pair of such objects are deemed equivalent

> witness terminal terminal :: EqC CatSet a b == Just Proxy

-}
    terminal = ObjSet [()]

instance Equivalence EqSet where
    type ObjEq EqSet a = ObjSet a

    witness ((ObjSet as) :: ObjSet a) ((ObjSet bs) :: ObjSet b) = do
        refl <- eqT :: Maybe (a :~: b)
        quiver refl as bs
        where
        quiver :: (a :~: b) -> [a] -> [b] -> Maybe (EqSet a b)
        quiver Refl as bs = do
            perm <- swap $ map (\a -> elemIndex a bs) as
            return $ EqSet perm
        -- ## check that this is a bijection

    -- reflect = 

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


