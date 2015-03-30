{-# LANGUAGE
        NoImplicitPrelude,
        GADTs,
        MultiParamTypeClasses,
        TypeFamilies,
        PolyKinds,
        ScopedTypeVariables,
        TypeOperators,
        ConstraintKinds
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
        -- * Categories
        Category(..), id,
        -- * Objects
        Product(..),
        CoProduct(..),
        Void
    ) where

-- alt-base modules: no dependencies, this is a root module

-- base modules
import Data.Bool (Bool(..))
import Data.Either (Either(..))
import Data.Typeable (Typeable, eqT, (:~:))
import Data.Maybe (Maybe(..))
import Control.Monad (Monad(..))
import GHC.Exts (Constraint)

-- compatibility modules
import Control.Arrow (Kleisli(..))

-- µPrelude
($) :: (a -> b) -> a -> b
($) f = \x -> f x
infixr 0 $
-- µPrelude

-----
-- Category class
-----

{- | Class of Categories.

Instances must satisfy the following laws:

prop> f . (idS f) == f
prop> (idT g) . g == g
prop> h . (g . f) == (h . g) . f
prop> g . f == bottom "if and only if" idS g `idEq` idT f

-}
class Category (hom :: k -> k -> *) where
    type ReflId hom (a :: k) (b :: k) :: Constraint
    
    idS     :: hom a b -> hom a a
    idT     :: hom a b -> hom b b
    idEq    :: ReflId hom a b
            => hom a a -> hom b b -> Bool

    (.)     :: hom b c -> hom a b -> hom a c

{- TODO:
class Category (hom :: k -> k -> *) where
    idS     :: hom a b -> hom a a
    IdT     :: hom a b -> hom b b

    <.>     :: Witness w b b'
            => w
            -> hom b' c -> hom a b
            -> hom a c

class Witness (w :: k -> k -> *) where
    witness :: a -> b -> Maybe (w a b)
-}


instance Category (->) where
    type ReflId (->) a b = (Typeable a, Typeable b)

    {-# INLINE idS #-}
    idS (f :: a -> b) = \x -> x :: a

    {-# INLINE idT #-}
    idT (f :: a -> b) = \x -> x :: b

    {-# INLINE idEq #-}
    idEq (f :: a -> a) (g :: b -> b) =
        case eqT :: Maybe (a :~: b) of
            Just _  -> True
            Nothing -> False

    {-# INLINE (.) #-}
    g . f = \x -> g (f x)

{- | Identity of Haskell functions, a unique polymorphic object.
-}
id :: a -> a
{-# INLINE id #-}
id = \x -> x


-----
-- Objects
-----

{- | Class of natural transformations from Arrow to Arrow.

A 'Transform' instance tranforms an object into another object, coming
with a functor which transforms functions into functions. Examples of
'Transform' instances are 'List' and 'Maybe'.

In the Haskell standard, this is (ill-)named the @Functor@ class. It is here
extended as an endofunctor in any Category.
-}
class Transform t where
    type Inv t a :: *
    type Inv t a = a

    tmap        :: Category f => f a b -> f (t a) (t b)

{- | Class of Products.

The prototypal instance of a product in Haskell is the tuple @(a,b)@. Any
type isomorphic to a 2-tuple can be viewed as a product.
-}
class Product p where
    type One p :: *

    fst     :: p a b -> a
    snd     :: p a b -> b
    
{- | Class of CoProducts.

The prototypal instance of a coproduct in Haskell is @Either a b@ representing
the sum of the types @a@ and @b@. Any type isomorphic to 'Either' can be viewed
as a coproduct.
-}
class CoProduct s where
    type Zero s :: *

    lft     :: a -> s a b
    rgt     :: b -> s a b

{- | Void is a type with no constructor.

Void is the zero object of the Haskell category and the neutral object
of coproducts in the Haskell category. Any type without constructors is
isomorphic to Void.
-}
data Void


-----
-- Instances
-----

instance Product (,) where
    type One (,) = ()

    fst (x,_)   = x
    snd (_,y)   = y
    
instance CoProduct Either where
    type Zero Either = Void

    rgt         = Right
    lft         = Left


-----
-- Compatibility section
-----

instance (Monad m) => Category (Kleisli m) where
    type ReflId (Kleisli m) a b = (Typeable a, Typeable b)

    {-# INLINE idS #-}
    idS (Kleisli (f :: a -> m b)) = Kleisli $ \x -> return (x :: a)
    
    {-# INLINE idT #-}
    idT (Kleisli (f :: a -> m b)) = Kleisli $ \x -> return (x :: b)

    {-# INLINE idEq #-}
    idEq
        (Kleisli (f :: a -> m a))
        (Kleisli (g :: b -> m b)) =
            case eqT :: Maybe (a :~: b) of
                Just _  -> True
                Nothing -> False

    {-# INLINE (.) #-}
    (Kleisli g) . (Kleisli f) = Kleisli (\x -> f x >>= g)

