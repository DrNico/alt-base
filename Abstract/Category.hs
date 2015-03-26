{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-|
Module          : Abstract.Category
Description     : Class of Categories, Functors and Natural Transformations
Copyright       : (c) Nicolas Godbout, 2015
License         : BSD-3
Maintainer      : nicolas.godbout@gmail.com
Stability       : Experimental
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
import Control.Arrow (Kleisli(..), (<<<))

-- needed standard modules
import qualified Data.Function (id, (.))
import Data.Either (Either(..))
import Data.Typeable (Proxy(..))
import Control.Monad (Monad(..))


-----
-- Categories: basic and flavours
-----

{- | Class of Categories.

Instances should satisfy the following laws:
     * if @target f == source g@, then @g . f@ is well-defined
     * if @source f == x@, then @f . id x == f@
     * if @target f == x@, then @id x . f == f@
     * if @h . g . f@ is well-defined, then @(h . g) . f == h . (g . f)@
-}
class Category (hom :: k -> k -> *) where
    type Object hom :: k -> *
    type Object hom = Proxy
    
    source          :: hom a b -> Object hom a
    target          :: hom a b -> Object hom b

    idC             :: Object hom a -> hom a a
    (.)             :: hom b c -> hom a b -> hom a c

{- | Identity function. 

This function is provided for compatibility in modules that use 'id' from
either the standard Prelude or the Control.Category module.
_All_ users of the prior versions of 'id' can use this function 
in its place.

Note that 'Category' instance definitions in user code
must still be modified to define 'idC'. _All_ such existing instances can
be obtained by changing
> id = <code>
to
> idC Proxy = <code>

The new 'Category' class can define a much larger class of categories with
the 'idC' method, in particular those taking an argument different from 'Proxy'.
-}
id :: (Category hom, Object hom ~ Proxy) => hom a a
id = idC Proxy

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
    const   :: a -> f (One p) a

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
    void    :: f a (Zero s)


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
    swap    :: p a b -> p b a

{- | Class of CoProducts.

The prototypal instance of a coproduct in Haskell is @Either a b@ representing
the sum of the types @a@ and @b@. Any type isomorphic to 'Either' can be viewed
as a coproduct.
-}
class CoProduct s where
    type Zero s :: *

    lft     :: a -> s a b
    rgt     :: b -> s a b
    coswap  :: s a b -> s b a

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

Functors of course form a Category over Categories, hence are also an instance
of Category.

Example:
This example shows that the Functor datatype can encode the @map@ function
for Lists. An endo-functor in (->), i.e., a record of
@ArrowFunctor (->) (->)@ has members

> pre   :: (a' -> (a -> b)) -> (a' -> b)
> post  :: (a -> (a -> b')) -> (a -> b')

Now suppose that primed types are the corresponding List type, such that 
@a' ~ List a@ and @b' ~ List b@. The record therefore has components

> pre   :: (a -> ([a] -> [b)) -> ([a] -> b)
> post  :: (a -> (a -> [b])) -> (a -> [b])

-}
data ArrowFunctor c d where
    ArrowFunctor :: (Category c, Category d) => {
        pre     :: c a' (d a b) -> d a' b,
        post    :: d a (c a b') -> c a b'
    } -> ArrowFunctor c d

instance Category ArrowFunctor where
    idC _ = ArrowFunctor {
            pre  = \c -> idC (source c),
            post = \d -> idC (source d)
        }

data MonoidalFunctor p c d where
    MonoidalFunctor :: {
        prem    :: ()
    } -> MonoidalFunctor p c d

-----
-- Type families magic
-----
-- These belong inside a 'Concrete' branch

-- canonical categories
data family C a b
data instance C a b                     = Arrow { runArrow :: a -> b }
data instance C (Proxy a) (Either e b)  = ErrorArrow { runEither :: a -> Either e b }
data instance C (a,r) (Proxy b)         = ReaderArrow { runReader :: (a,r) -> b }
data instance C (a,s) (b,s)             = StateArrow { runState :: (a,s) -> (b,s) }


-- functors
data family F f
--data instance F ()          -- identity functor
--data instance F Product     -- Monoidal functor over the product
--data instance F CoProduct   -- CoMonoidal functor

-----
-- Instances
-----

{-
The Category of Haskell functions has a single class of objects.
This class is best represented by the Haskell type 'Proxy', which
enables type inspection and reflection.
-}
instance Category (->) where
    type Object (->) = Proxy

    source _ = Proxy
    target _ = Proxy

    idC _ = Data.Function.id
    (.)  = (Data.Function..)

instance Product (,) where
    type One (,) = ()

    fst (x,_)   = x
    snd (_,y)   = y
    swap (x,y)  = (y,x)

instance CoProduct Either where
    type Zero Either = Void

    rgt         = Right
    lft         = Left
    coswap (Left a)     = Right a
    coswap (Right b)    = Left b


-----
-- Compatibility section
-----

instance (Monad m) => Category (Kleisli m) where
    type Object (Kleisli m) = Proxy
    
    source _ = Proxy
    target _ = Proxy

    idC _ = Kleisli return
    (.)  = (<<<)



{-
instance (Category cat, Applicative f) => Category (Ap2 f cat) where
    type Ob (Ap2 f cat) = Ap1 f (Ob cat)

    source (Ap2 f) = Ap1 (fmap source f)
    target (Ap2 f) = Ap1 (fmap target f)

    id (Ap1 x) = Ap2 (fmap id x)

    Ap2 g . Ap2 f =
        Ap2 $ pure (.) <*> g <*> f

newtype Ap1 f g a = Ap1 {
    unAp1 :: f (g a)
}
newtype Ap2 f g a b = Ap2 {
    unAp2 :: f (g a b)
}

-----
-- Functor between Categories
-----

class Functor c d where
    type OMap c d a :: *
    type FMap c d a b :: *

    omap  :: (Category c, Category d)
          => (Ob c) a -> OMap c d a
    mmap  :: (Category c, Category d)
          => c a b -> FMap c d a b

instance (Category cat, Applicative f) => Functor cat (Ap2 f cat) where
    type OMap cat (Ap2 f cat) a = Ap1 f (Ob cat) a
    type FMap cat (Ap2 f cat) a b = Ap2 f cat a b

    omap x = Ap1 (pure x)
    mmap f = Ap2 (pure f)

-}