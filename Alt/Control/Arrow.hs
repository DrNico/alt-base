
{-|
Module:             Control.Arrow
Description:        Definition of Arrow class and dependents
Copyright:          (c) 2002 Ross Paterson
                    (c) 2015 Nicolas Godbout
Licence:            BSD-3
Stability:          experimental
Portability:        portable

Definition of arrows, based on

  * /Generalising Monads to Arrows/, by John Hughes,
    /Science of Computer Programming/ 37, pp67-111, May 2000.

plus a couple of definitions ('returnA' and 'loop') from

  * /A New Notation for Arrows/, by Ross Paterson, in /ICFP 2001/,
    Firenze, Italy, pp229-240.

These papers and more information on arrows can be found at
<http://www.haskell.org/arrows/>.

As a historical note, this module is a rewrite of the original Control.Arrow module
from Ross Paterson, preserving compatibility with the original
while relaxing class dependency constraints.
-}

module Control.Arrow (
    -- * Arrows
    PreArrow(..),
    Arrow(..),
    -- ** compatibility combinators
    arr, first, second, (***), (&&&),
    -- ** derived combinators
    returnA,
    (>>>), (<<<),
    -- * Arrows from a Monad
    Kleisli(..),
    -- * Monoid operations
    ArrowZero(..), ArrowPlus(..),
    -- * Conditionals
    ArrowChoice(..),
    -- ** compatibility combinators
    left, right, (+++), (|||),
    -- * Arrow application,
    ArrowApply(..),
    -- * Feedback
    ArrowLoop(..)
    ) where

import Control.Category (Category(..), (<<<), (>>>))
import Control.Monad (Monad(..))
import Data.Either (Either(..))

import Prelude (($), const, undefined)

infixr 5 <+>
infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 |||
-- infixr 1 ^>>, >>^
-- infixr 1 ^<<, <<^

{- | The 'PreArrow' class is the root of all the following classes.
It allows extraction and insertion of values into Categories that
support it.
-}
class Category f => PreArrow f where
    pull :: (a -> f () b) -> f a b
    push :: a -> f () a

{-| The basic arrow class, as historically defined in the papers.

Minimal complete definition: @pullProd@, @pushProd@

prop> pull (\x -> push x) = id
prop> swap . swap = id

-}
class PreArrow f => Arrow f where
    pullProd :: ((a,b) -> f () c) -> f (a,b) c
    pushProd :: (f () b, f () c) -> f () (b,c)
    
-----
-- Compatibility combinators
-----

arr :: PreArrow f => (a -> b) -> f a b
arr f = pull (\a -> push (f a))

first :: Arrow f => f a b -> f (a,c) (b,c)
first f = pullProd (\(a,b) -> pushProd (f . push a, push b))

second :: Arrow f => f a b -> f (c,a) (c,b)
second f = pullProd (\(a,b) -> pushProd (push a, f . push b))

(***) :: Arrow f => f a b -> f a' b' -> f (a,a') (b,b')
f *** g = pullProd (\(a,b) -> pushProd (f . push a, g . push b))

(&&&) :: Arrow f => f a b -> f a b' -> f a (b,b')
f &&& g = pull (\a -> pushProd (f . push a, g . push a))

-----
-- Derived combinators
-----

-- | The identity arrow, which plays the role of 'return' in arrow notation.
returnA :: Arrow f => f a a
returnA = id

-----
-- Kleisli arrows
-----

-- | Kleisli arrows of a monad.
newtype Kleisli m a b = Kleisli {
    runKleisli :: a -> m b
}

-----
-- Monoid operations
-----

class Arrow arr => ArrowZero arr where
    zeroArrow :: arr b c

class Arrow arr => ArrowPlus arr where
    (<+>) :: arr a b -> arr a b -> arr a b

-----
-- Conditionals
-----

{- | Choice, for arrows that support it.
-}
class PreArrow arr => ArrowChoice arr where
    copull      :: (Either a b -> arr () c) -> arr (Either a b) c
    copush      :: Either (arr a b) (arr a c) -> arr a (Either b c)


left :: ArrowChoice arr => arr a b -> arr (Either a c) (Either b c)
left f = f +++ id

right :: ArrowChoice arr => arr b c -> arr (Either a b) (Either a c)
right f = id +++ f

(+++) :: ArrowChoice arr => arr a b -> arr a' b' -> arr (Either a a') (Either b b')
f +++ g = copull $ \x -> copush $ case x of
            Left a  -> Left $ push a >>> f
            Right b -> Right $ push b >>> g

(|||) :: ArrowChoice arr => arr a c -> arr b c -> arr (Either a b) c
f ||| g = copull $ \x -> case x of
            Left a  -> push a >>> f
            Right b -> push b >>> g

-----
-- Arrow application
-----

-- With the new scheme, all arrows are instances of ArrowApply.
class Arrow arr => ArrowApply arr where
    app :: arr (arr a b, a) b

-----
-- Feedback
-----

class Arrow arr => ArrowLoop arr where
    loop    :: arr (a, c) (b, c) -> arr a b

-----
-- Instances
-----

instance PreArrow (->) where
    pull f = \a -> f a ()
    push a = \() -> a

instance Arrow (->) where
    pullProd f  = \(a,b) -> f (a,b) ()
    pushProd (f,g)  = \() -> (f (),g ())


instance ArrowChoice (->) where
    copull f           = \x -> f x ()

    copush (Left f)    = \x -> Left (f x)
    copush (Right g)   = \x -> Right (g x)


instance ArrowApply (->) where
    app (f,x) = f x


instance Monad m => Category (Kleisli m) where
    id = Kleisli return
    g . f = Kleisli $ \x -> runKleisli f x >>= runKleisli g

instance Monad m => PreArrow (Kleisli m) where
    pull f = Kleisli $ \a -> runKleisli (f a) ()
    push a = Kleisli $ \() -> return a


instance Monad m => Arrow (Kleisli m) where


instance Monad m => ArrowChoice (Kleisli m) where





