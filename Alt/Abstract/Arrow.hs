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
Module          : Alt.Abstract.Arrow
Description     : Core Arrow classes
Copyright       : (c) Nicolas Godbout, 2015
                  (c) Ross Patterson, 2002
License         : BSD-3
Maintainer      : nicolas.godbout@gmail.com

= Arrow

-}
module Alt.Abstract.Arrow (
        {- * General Arrow Templates
        The following classes form a family of arrows parametrized by a /n/-ary
        type function. Remarkably, these subsume all operations of the original
        @arrows@ package and are no longer restricted to Haskell categories.
        These definitions also lift restrictions on arrow transformers.
        -}
        Arrow_(..),
        Arrow0(..),
        Arrow1(..),
        Arrow2(..),
        Arrow3(..)
    ) where

import Alt.Abstract.Category

-- base modules
import Control.Applicative (Applicative(..))
import Data.Either (Either(..))

import Alt.Prelude

{-
## TODO: reframe these into slices and coslices ?
-}

{- | Template of an Arrow that may be defined by Haskell-level lambda
expressions.


-}
class (Category f) => Arrow_ f where
    lambda      :: (a -> f () b) -> f a b
    const       :: b -> f a b

{- | Template of a pointed Arrow involving a 0-ary type function, /i.e./,
a point or equivalently a type. The main difference with the 'Arrow_' class is 
the presence of a type parameter, which makes this a pointed arrow.
-}
class (Category f) => Arrow0 t f where
    pull0       :: (t -> f () b) -> f t b    -- f One b
    push0       :: t -> f a t           -- f Zero One
    -- there is something deep here!

{- | Template of an Arrow involving a 1-ary type function. It forms the
basis of 'fold' and 'unfold' functions.
-}
class (Category f) => Arrow1 t f where
    pull1       :: (t a -> f () b) -> f (t a) b
    push1       :: t (a -> b) -> f a (t b)
    -- if t == Id then pull1 == push1 ~ arr

{- | Template of an Arrow involving a 2-ary type function.

Inspecting its type yields enlightenment about products and coproducts
in a category. In pseudo-Haskell, there is an equivalence of /classes/

> Arrow2 (,) f ~ ArrowProduct f
> Arrow2 Either f ~ ArrowSum f

If @t@ is a Product, then the category @f@ is Monoidal.
If @t@ is a CoProduct, equivalently a Sum, then the category @f@ is 
CoMonoidal.
-}
class (Category f) => Arrow2 t f where
    pull2       :: (t a b -> f () c) -> f (t a b) c
    push2       :: t (f a b) (f a c) -> f a (t b c)
    -- if t is a Product, then this Category is Monoidal
    -- if t is a Sum, then this Category is CoMonoidal

{- | Template for an arrow involving a 3-ary type function.
-}
class (Category f) => Arrow3 t f where
    pull3       :: (t a b c -> f () d) -> f (t a b c) d
    push3       :: t (a -> b) (a -> c) (a -> d) -> f a (t b c d)

-- COMMENT: since we would like the transformer 't' to be PolyKind,
-- have /it/ transform Arrows into Arrows.


-----
-- Instances
-----

----- Instances for (->) -----

instance Arrow_ (->) where
    {-# INLINE lambda #-}
    lambda f = \x -> f x ()

    {-# INLINE const #-}
    const x = \_ -> x

instance Arrow0 t (->) where
    pull0 f = \x -> f x ()
    push0 t = \_ -> t

instance Applicative t => Arrow1 t (->) where
    pull1 f = \x -> f x ()
    push1 = \f -> \a -> f <*> pure a

instance Arrow2 (,) (->) where
    pull2 f = \x -> f x ()
    push2 = lambda
        where lambda (f,g) = \a -> (f a, g a)

instance Arrow2 Either (->) where
    pull2 f = \x -> f x ()
    push2 = lambda
        where
            lambda (Left f)  = \a -> Left (f a)
            lambda (Right g) = \b -> Right (g b)

