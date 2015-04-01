{-# LANGUAGE
        NoImplicitPrelude,
        TypeFamilies,
        PolyKinds,
        GADTs,
        MultiParamTypeClasses,
        FlexibleInstances
  #-}

{- |
Module          : Abstract.Arrow
Description     : Arrow Categories
Copyright       : (c) Nicolas Godbout, 2015
                  (c) Ross Patterson, 2002
License         : BSD-3
Maintainer      : nicolas.godbout@gmail.com

=1 Arrow
=2 Overview
This is the greatest module ever. See
<http://www.greatest.ever/paper/>

=2 Compatibility
This module fully supports GHC's arrow notation. Import it instead of
"Control.Arrow" and add the following pragma
at the head of the modules importing this one.

> {-# LANGUAGE Arrows, RebindableSyntax #-}

Note that this implies @NoImplicitPrelude@, so you may have to add

> import Prelude (&#3C;/used symbols/&#3E;)

with the list of symbols that are used.

User-defined instances of the old Arrow class and its derivatives have
to be redefined to be compatible with this package.

=2 Historical Note
The original formulation of arrows is exposed in

  * /Generalising Monads to Arrows/, John Hughes, /Science
    of Computer Programming/ *37*, 67-111 (2000).
  * /A New Notation for Arrows/, Ross Patterson, /in/ ICFP 2001,
    229-240, Firenze, Italy.

These papers and more information on arrows can be found at
<http://www.haskell.org/arrows/>.
-}

module Alt.Arrow (
        -- * Simple Arrows
        -- ** The root Arrow class
        Arrow(..),
        -- ** Arrow with state
        ArrowProd(..),
        -- *** compatibility functions
        first, second, (***), (&&&),
        -- ** Arrow with choice
        ArrowSum(..),
        -- *** compatibility functions
        left, right, (+++), (|||),
        -- ** Applicative Arrows
        ArrowApply(..),
        -- ** Loops
        ArrowLoop(..),
        -- * Arrow Transformer
        ArrowTrans(..),
        -- * Arrow Templates
        Arrow0(..),
        Arrow1(..),
        Arrow2(..)
    ) where

-- alt-base modules
import Alt.Category
import Alt.Object.Identity

-- base modules
import Control.Applicative (Applicative(..))
import Data.Either (Either(..))

-- alt-base Prelude
import Alt.Prelude


infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 |||

-- TODO: Arrows should really be defined over LARGE categories.

{- | The basic Arrow class.
-}
class LargeCategory f => Arrow f where
    arr         :: (a -> f () b) -> f a b
    const       :: a -> f () a

{- | The class of arrows over tuples. This allows an arrow to carry a state.
-}
class Arrow f => ArrowProd f where
    pull        :: ((a,b) -> f () c) -> f (a,b) c
    push        :: (f a b, f a c) -> f a (b,c)

-- These should be (in pseudo-Haskell)
--  pull        :: (a -> f b* c) -> f (a b*) c
--  push        :: a -> f b* (a b*)


-- | Send the first component of the input through the argument
--   arrow, and copy the rest unchanged to the output.
first :: ArrowProd a => a b c -> a (b,d) (c,d)
{-# INLINE first #-}
first f = pull $ \(x,y) -> push (f . const x, const y)

-- | A mirror image of 'first'.
second :: ArrowProd a => a b c -> a (d,b) (d,c)
{-# INLINE second #-}
second f = pull $ \(x,y) -> push (const x, f . const y)

-- | Split the input between the two argument arrows and combine
--   their output.  Note that this is in general not a functor.
(***) :: ArrowProd a => a b c -> a b' c' -> a (b,b') (c,c')
{-# INLINE (***) #-}
f *** g = pull $ \(x,y) -> push (f . const x, g . const y)

-- | Fanout: send the input to both argument arrows and combine
--   their output.
(&&&) :: ArrowProd a => a b c -> a b c' -> a b (c,c')
{-# INLINE (&&&) #-}
f &&& g = arr $ \x -> push (f . const x, g . const x)


{- | The class of arrows with choice.
-}
class Arrow f => ArrowSum f where
    copull      :: (Either a b -> f () c) -> f (Either a b) c
    copush      :: Either (f a b) (f a c) -> f a (Either b c)

-- These should be (in pseudo-Haskell)
--  copull      :: (a -> f b+ c) -> f (a + b+) c
--  copush      :: (f a b + f a c) -> f a (b + c)

-- | Feed marked inputs through the argument arrow, passing the
--   rest through unchanged to the output.
left :: ArrowSum a => a b c -> a (Either b d) (Either c d)
{-# INLINE left #-}
left f = copull lambda
    where
        lambda (Left x)  = copush (Left (f . const x))
        lambda (Right y) = copush (Right (const y))

-- | A mirror image of 'left'.
right :: ArrowSum a => a b c -> a (Either d b) (Either d c)
{-# INLINE right #-}
right f = copull lambda
    where
        lambda (Left x)  = copush (Left (const x))
        lambda (Right y) = copush (Right (f . const y))

-- | Split the input between two argument arrows.
(+++) :: ArrowSum a => a b c -> a b' c' -> a (Either b b') (Either c c')
{-# INLINE (+++) #-}
f +++ g = copull lambda
    where
        lambda (Left x)  = copush (Left (f . const x))
        lambda (Right y) = copush (Right (g . const y))

-- | Fanin: feed the input to one of the argument arrows.
(|||) :: ArrowSum a => a b d -> a c d -> a (Either b c) d
{-# INLINE (|||) #-}
f ||| g = copull lambda
    where
        lambda (Left x)  = f . const x
        lambda (Right y) = g . const y


class Arrow arr => ArrowApply arr where
    app :: arr (arr a b, a) b

class Arrow arr => ArrowLoop arr where
    loop :: arr (e,a) (e,b) -> arr a b

{- | An Arrow Transformer constructs an arrow from an inner one.
-}
class (Arrow arr, Arrow (f arr)) => ArrowTrans f arr where
    liftA       :: arr a b -> (f arr) a b



-- t is a nullary transformer, hence Set
{- | Template of an Arrow involving a 0-ary type function. Inspecting its type
can bring enlightenment about theoretical category theory, having to
do with initial and final objects.
-}
class (LargeCategory f) => Arrow0 t f where
    pull0       :: (t -> b) -> f t b    -- const b
    push0       :: t -> f a t
    -- if t is a Product, then push0 == const One
    -- if t is a Sum, then push0 == Zero, i.e. 'fail'

{- | Template of an Arrow involving a 1-ary type function. It forms the
basis of 'fold' and 'unfold' functions.
-}
class (LargeCategory f) => Arrow1 t f where
    pull1       :: (t a -> b) -> f (t a) b
    push1       :: t (a -> b) -> f a (t b)
    -- if t == Id then pull1 == push1 ~ arr

{- | Template of an Arrow involving a 2-ary type function. Inspecting its
type yields enlightenment about products and coproducts in a category.
-}
class (LargeCategory f) => Arrow2 t f where
    pull2       :: (t a b -> c) -> f (t a b) c
    push2       :: t (a -> b) (a -> c) -> f a (t b c)
    -- if t is a Product, then this Category is Monoidal
    -- if t is a Sum, then this Category is CoMonoidal

-- COMMENT: since the transformer 't' is PolyKind, have /it/ transform
-- Arrows into Arrows.


-----
-- Instances
-----

instance Arrow (->) where
    {-# INLINE arr #-}
    arr f = \x -> f x ()

    {-# INLINE const #-}
    const x = \_ -> x

instance ArrowProd (->) where
    {-# INLINE pull #-}
    pull f = \x -> f x ()

    {-# INLINE push #-}
    push (f,g) = \x -> (f x, g x)

instance ArrowSum (->) where
    {-# INLINE copull #-}
    copull f = \x -> f x ()

    {-# INLINE copush #-}
    copush (Left f)  = \x -> Left (f x)
    copush (Right g) = \y -> Right (g y)

instance ArrowApply (->) where
    app = \(f,a) -> f a

instance ArrowLoop (->) where
    loop f = \x -> let (e,y) = f (e,x) in y


instance Arrow0 t (->) where
    pull0 = id
    push0 _ = \_ -> error "Abstract.Arrow.push0"

instance Applicative t => Arrow1 t (->) where
    pull1 = id
    push1 = \f -> \a -> f <*> pure a

-- generalize to Applicative functors
instance Arrow1 Id (->) where
    pull1 = id
    push1 = lambda
        where lambda (Id f) = Id . f

instance Arrow2 (,) (->) where
    pull2 = id
    push2 = lambda
        where lambda (f,g) = \a -> (f a, g a)

instance Arrow2 Either (->) where
    pull2 = id
    push2 = lambda
        where
            lambda (Left f)  = \a -> Left (f a)
            lambda (Right g) = \b -> Right (g b)




