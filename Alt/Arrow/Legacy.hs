{-# LANGUAGE
        NoImplicitPrelude,
        GADTs,
        PolyKinds,
        TypeFamilies,
        TypeOperators,
        ConstraintKinds,
        FlexibleContexts,
        FlexibleInstances,
        ScopedTypeVariables,
        MultiParamTypeClasses
    #-}

{- |
Module          : Alt.Arrow.Legacy
Description     : Adapted original @arrows@ package
Copyright       : (c) Nicolas Godbout, 2015
                  (c) Ross Patterson, 2002
License         : BSD-3
Maintainer      : nicolas.godbout@gmail.com

= Legacy Arrows
This module provides an almost identical interface to Arrows than the
original 'Control.Arrow' module by Ross Patterson, but where every
class and method is defined over the "Alt.Abstract.Arrow" classes. It serves
two purposes:

  1. Serve as a compatibility module for arrow do-notation and for existing
     code using arrows.
  2. Constitute proof that this package's reformulation of Arrows is at least
     as expressive as the original.

= Compatibility
This module fully supports GHC's arrow notation. Import it instead of
"Control.Arrow" and add the following pragma
at the head of the modules importing this one.

> LANGUAGE Arrows, RebindableSyntax

Note that this implies @NoImplicitPrelude@, so you may have to add

> import Prelude (<symbols>)

with the list of symbols that are used.

User-defined instances of the old Arrow class and its derivatives have
to be redefined to be compatible with this package.

= Historical Note
The original formulation of arrows is exposed in

  * /Generalising Monads to Arrows/, John Hughes,
    /Science of Computer Programming/ __37__, 67-111 (2000).
  * /A New Notation for Arrows/, Ross Patterson, /in/ ICFP 2001,
    229-240, Firenze, Italy.

These papers and more information on arrows can be found at
<http://www.haskell.org/arrows/>.
-}

module Alt.Arrow.Legacy (
        -- * Legacy Arrows
        -- ** The root Arrow class
        Arrow(..),
        -- *** compatibility functions
        arr, first, second, (***), (&&&),
        -- ** Arrow with choice
        ArrowChoice(..),
        -- *** compatibility functions
        left, right, (+++), (|||),
        -- ** Applicative Arrows
        ArrowApply(..),
        -- ** Loops
        ArrowLoop(..),
        -- * Arrow Transformer
        ArrowTrans(..),
    ) where

-- alt-base modules
import Alt.Abstract.Category
import Alt.Abstract.Arrow

-- base modules
import Data.Either (Either(..))
import Data.Typeable ((:~:), Proxy(..))
import GHC.Exts (Constraint)

-- alt-base Prelude
import Alt.Prelude


infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 |||

{- | Arrow class, constituted from an identity lambda-builder and
a tuple as product.
-}
type Arrow f = (Haskell f, Arrow_ f, Arrow2 (,) f)

{- | Lift a pure function into an arrow.
-}
arr :: Arrow f => (a -> b) -> f a b
arr f = lambda $ \x -> const (f x)

-- | Send the first component of the input through the argument
--   arrow, and copy the rest unchanged to the output.
first :: (Arrow a)
      => a b c -> a (b,d) (c,d)
{-# INLINE first #-}
first f = pull2 $ \(x,y) -> push2 (f . const x, const y)

-- | A mirror image of 'first'.
second :: (Arrow a)
       => a b c -> a (d,b) (d,c)
{-# INLINE second #-}
second f = pull2 $ \(x,y) -> push2 (const x, f . const y)

-- | Split the input between the two argument arrows and combine
--   their output.  Note that this is in general not a functor.
(***) :: (Arrow a)
      => a b c -> a b' c' -> a (b,b') (c,c')
{-# INLINE (***) #-}
f *** g = pull2 $ \(x,y) -> push2 (f . const x, g . const y)

-- | Fanout: send the input to both argument arrows and combine
--   their output.
(&&&) :: (Arrow a)
      => a b c -> a b c' -> a b (c,c')
{-# INLINE (&&&) #-}
f &&& g = lambda $ \x -> push2 (f . const x, g . const x)


{- 
type ArrowChoice f  = (Arrow1 Id f, Arrow2 Either f)
type ArrowApply f   = (Arrow1 Id f, Arrow2 (,) f, Arrow2 f f)

-}



{- | The class of arrows with choice.
-}
-- class Arrow f => ArrowSum f where
--     copull      :: (Either a b -> f () c) -> f (Either a b) c
--     copush      :: Either (f a b) (f a c) -> f a (Either b c)

type ArrowChoice f = (Haskell f, Arrow_ f, Arrow2 Either f)

-- | Feed marked inputs through the argument arrow, passing the
--   rest through unchanged to the output.
left :: (ArrowChoice a)
     => a b c -> a (Either b d) (Either c d)
{-# INLINE left #-}
left f = pull2 lambda
    where
        lambda (Left x)  = push2 (Left (f . const x))
        lambda (Right y) = push2 (Right (const y))

-- | A mirror image of 'left'.
right :: (ArrowChoice a)
      => a b c -> a (Either d b) (Either d c)
{-# INLINE right #-}
right f = pull2 lambda
    where
        lambda (Left x)  = push2 (Left (const x))
        lambda (Right y) = push2 (Right (f . const y))

-- | Split the input between two argument arrows.
(+++) :: (ArrowChoice a)
      => a b c -> a b' c' -> a (Either b b') (Either c c')
{-# INLINE (+++) #-}
f +++ g = pull2 lambda
    where
        lambda (Left x)  = push2 (Left (f . const x))
        lambda (Right y) = push2 (Right (g . const y))

-- | Fanin: feed the input to one of the argument arrows.
(|||) :: (ArrowChoice a)
      => a b d -> a c d -> a (Either b c) d
{-# INLINE (|||) #-}
f ||| g = pull2 lambda
    where
        lambda (Left x)  = f . const x
        lambda (Right y) = g . const y

{- | Arrows allowing application of a first-class arrow to another input.
-}
class Arrow arr => ArrowApply arr where
    app :: arr (arr a b, a) b

{- | The 'loop' operator expresses computations where an output value is fed
back as input.
-}
class Arrow arr => ArrowLoop arr where
    loop :: arr (a,c) (b,c) -> arr a b

{- | An Arrow Transformer constructs an arrow from an inner one.
-}
class (Arrow arr, Arrow (f arr)) => ArrowTrans f arr where
    liftA       :: arr a b -> (f arr) a b

