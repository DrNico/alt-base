
{-|
Module:             Control.Arrow
Description:        Definition of Arrow class and dependents
Copyright:          (c) 2002 Ross Paterson
                    (c) 2015 Nicolas Godbout
Licence:            BSD-3
Stability:          stable
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
    -- * Arrow application,
    ArrowApply(..),
    -- * Feedback
    ArrowLoop(..)
    ) where

import Control.Category (Category(..), (<<<), (>>>))
import Control.Monad (Monad(..))
import Data.Either (Either(..))

import Prelude (($), undefined)

infixr 5 <+>
infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 |||
-- infixr 1 ^>>, >>^
-- infixr 1 ^<<, <<^

{-| The basic arrow class.

Minimal complete definition: @pull@, @push@ and @swap@ satisfying the laws
prop> pull (\x -> push x) = id
prop> swap . swap = id
-}
class Category f => Arrow f where
    pull :: (a -> f x y) -> f (x,a) y
         -- a -> b -> f () c
    push :: a -> f (x,()) (x,a)
         -- f a b -> f a c -> f a (b,c)
         -- prop> pull (\x y -> push (const x) (const y)) == id
    incr :: f a (a,())
    decr :: f (a,()) a
    swap :: f (a,b) (b,a)

-----
-- Compatibility combinators
-----

arr :: Arrow f => (a -> b) -> f a b
arr f = incr >>> first (arr f) >>> decr

first :: Arrow f => f a b -> f (a,c) (b,c)
first f = pull (\c -> f >>> incr >>> push c)

second :: Arrow f => f a b -> f (c,a) (c,b)
second f = swap >>> first f >>> swap

(***) :: Arrow f => f a b -> f a' b' -> f (a,a') (b,b')
f *** g = first f >>> second g

(&&&) :: Arrow f => f a b -> f a b' -> f a (b,b')
f &&& g = dup >>> first f >>> second g
    where
        dup = incr >>> swap >>> pull (\x -> incr >>> push x >>> swap >>> push x)

-----
-- Derived combinators
-----

-- | The identity arrow, which plays the role of 'return' in arrow notation.
returnA :: Arrow f => f a a
returnA = id

const :: Arrow f => a -> f () a
const a = incr >>> pull (\_ -> incr >>> push a >>> swap >>> decr)

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
class Arrow arr => ArrowChoice arr where
    copull      :: (Either a b -> arr () c) -> arr (Either a b) c
    copush      :: Either (arr a b) (arr a c) -> arr a (Either b c)
    coswap      :: arr (Either a b) (Either b a)


left :: ArrowChoice arr => arr a b -> arr (Either a c) (Either b c)
left f = f +++ id

right :: ArrowChoice arr => arr b c -> arr (Either a b) (Either a c)
right f = id +++ f

(+++) :: ArrowChoice arr => arr a b -> arr a' b' -> arr (Either a a') (Either b b')
f +++ g = copull $ \x -> copush $ case x of
            Left a  -> Left $ const a >>> f
            Right b -> Right $ const b >>> g

(|||) :: ArrowChoice arr => arr a c -> arr b c -> arr (Either a b) c
f ||| g = copull $ \x -> case x of
            Left a  -> const a >>> f
            Right b -> const b >>> g

-----
-- Arrow application
-----

-- With the new scheme, all arrows are instances of ArrowApply.
class Arrow arr => ArrowApply arr where
    app :: arr (arr a b, a) b
    app = swap >>> pull id

-----
-- Feedback
-----

class Arrow arr => ArrowLoop arr where
    loop    :: arr (a, c) (b, c) -> arr a b

-----
-- Instances
-----

instance Arrow (->) where
    pull f = \(x,a) -> f a x
    push a = \(x,()) -> (x,a)
    swap   = \(x,y) -> (y,x)
    incr   = \x -> (x,())
    decr   = \(x,()) -> x


instance ArrowChoice (->) where
    copull f           = \x -> f x ()

    copush (Left f)    = \x -> Left (f x)
    copush (Right g)   = \x -> Right (g x)
    
    coswap (Right a)   = Left a
    coswap (Left a)    = Right a


instance ArrowApply (->) where
    app (f,x) = f x


instance Monad m => Category (Kleisli m) where
    id = Kleisli return
    g . f = Kleisli $ \x -> runKleisli f x >>= runKleisli g


instance Monad m => Arrow (Kleisli m) where
    pull f = Kleisli $ \(x,a) -> runKleisli (f a) x
    push a = Kleisli $ \(x,()) -> return (x,a)
    swap   = Kleisli $ \(x,y) -> return (y,x)
    incr   = Kleisli $ \x -> return (x,())
    decr   = Kleisli $ \(x,()) -> return x


instance Monad m => ArrowChoice (Kleisli m) where
    copull f           = Kleisli $ \x -> runKleisli (f x) ()

    copush (Left f)    = Kleisli $ \x -> runKleisli f x >>= return . Left
    copush (Right f)   = Kleisli $ \x -> runKleisli f x >>= return . Right

    coswap             = Kleisli $ return . coswap





