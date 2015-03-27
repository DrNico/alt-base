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
Description     : Class of Categories, Functors and Natural Transformations
Copyright       : (c) Nicolas Godbout, 2015
License         : BSD-3
Maintainer      : nicolas.godbout@gmail.com
-}
module Abstract.Category (
        -- * Categories
        Category(..), id,
        Monoidal(..),
        CoMonoidal(..),
        -- * Objects
        Product(..),
        CoProduct(..),
        Void,
        -- * Functors
        ArrowFunctor(..)
    ) where

-- alt-base modules: no dependencies, this is a root module

-- compatibility modules
import Control.Arrow (Kleisli(..))

-- needed standard modules
import Data.Bool (Bool(..))
import Data.Either (Either(..))
import Data.Typeable (Typeable, eqT, (:~:))
import Data.Maybe (Maybe(..))
import Control.Monad (Monad(..))
import GHC.Exts (Constraint)

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
    -- UndecideableInstances required to accept (k -> k -> Constraint)

    idS     :: hom a b -> hom a a
    idT     :: hom a b -> hom b b
    idEq    :: ReflId hom a b
            => hom a a -> hom b b -> Bool

    (.)     :: hom b c -> hom a b -> hom a c


instance Category (->) where
    type ReflId (->) a b = (Typeable a, Typeable b)

    -- the only possible issue with these is that Constraints are lost
    idS (f :: a -> b) = \x -> x :: a
    idT (f :: a -> b) = \x -> x :: b
    idEq (f :: a -> a) (g :: b -> b) =
        case eqT :: Maybe (a :~: b) of
            Just _  -> True
            Nothing -> False

    g . f = \x -> g (f x)

{- | Identity of Haskell functions, a unique object.
-}
id :: a -> a
id = \x -> x
{-# INLINE id #-}


{- | Natural Category

Class of Categories arising from a natural transformation, starting
from any base Category. The class methods provide helpers to build
folding and unfolding arrows.
-}
class (Category f, Transform t) => Natural f t where
    mkfold      :: (t a -> f () b) -> f (t a) b
    mkunfold    :: (t (f a b)) -> f a (t b)

{- | Monoidal Category

Class of Categories having a product. The prototypal example of such
a category is one where arrows can manipulate tuples.
-}
class (Category f, Product p) => Monoidal f p where
    pull    :: (p a b -> f (One p) c) -> f (p a b) c
    push    :: p (f a b) (f a c) -> f a (p b c)
    
{- | CoMonoidal Category

Class of Categories having a coproduct, sometimes also called a sum.
The prototypal example of such a category is one with choice, such
as programming languages having control-flow, i.e., if-then-else or
an equivalent. In Haskell, the 'Either' type is the quintessential
coproduct.
-}
class (Category f, CoProduct s) => CoMonoidal f s where
    copull  :: (s a b -> f (Zero s) c) -> f (s a b) c
    copush  :: s (f a b) (f a c) -> f a (s b c)


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
-- Functors: basic and flavours
-----

{- | Prototypal functor from Category @c@ to Category @d@

Functors form a Category over Categories, hence are also an instance
of Category.

Example:
This example shows that the Functor datatype can encode the @map@ function
for Lists. An endo-functor in (->), i.e., a record of
@ArrowFunctor (->) (->)@ has members

> pre   :: (a' -> (a -> b)) -> (a' -> b)
> post  :: (a -> (a -> b')) -> (a -> b')

Now suppose that primed types are the corresponding List type, such that 
@a' ~ List a@ and @b' ~ List b@. The record therefore has components

> pre   :: ([a] -> ([] -> b)) -> ([a] -> b)
> post  :: (a -> (a -> [b])) -> (a -> [b])

-}
data ArrowFunctor c d where
    ArrowFunctor :: (Category c, Category d) => {
        pre     :: c a' (d a b) -> d a' b,
        post    :: d a (c a b') -> c a b'
    } -> ArrowFunctor c d

{-
instance Category ArrowFunctor where
    idC _ = ArrowFunctor {
            pre  = \c -> idC (source c),
            post = \d -> idC (source d)
        }

-}

data MonoidalFunctor p c d where
    MonoidalFunctor :: {
        prem    :: ()
    } -> MonoidalFunctor p c d


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

    idS (Kleisli (f :: a -> m b)) = Kleisli $ \x -> return (x :: a)
    idT (Kleisli (f :: a -> m b)) = Kleisli $ \x -> return (x :: b)
    idEq
        (Kleisli (f :: a -> m a))
        (Kleisli (g :: b -> m b)) =
            case eqT :: Maybe (a :~: b) of
                Just _  -> True
                Nothing -> False

    (Kleisli g) . (Kleisli f) = Kleisli (\x -> f x >>= g)

