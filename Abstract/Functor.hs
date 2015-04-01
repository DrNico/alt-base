{-# LANGUAGE
        NoImplicitPrelude,
        GADTs,
        PolyKinds,
        TypeFamilies,
        TypeOperators,
        ConstraintKinds,
        ScopedTypeVariables,
        MultiParamTypeClasses
  #-}

{-|
Module          : Abstract.Functor
Description     : Class of Functors taking Categories to Categories
Copyright       : (c) Nicolas Godbout, 2015
License         : BSD-3
Maintainer      : nicolas.godbout@gmail.com

A /functor/ maps morphisms from one category to morphisms into another category.
If the source and target categories are the same, it is called an /endofunctor/.

Functors look like categories themselves, having a source and a target, and being
composable. Hence, every functor is an instance of the 'Category' class.

A concrete functor is an instance of a datatype. This module does not define
a @Functor@ class, nor any other class. It contains datatype definitions having
all the properties of /being/ a functor: having categories as source and target, 
as well as being categories themselves.

See an Abstract.Functor#Example.
-}


-----
-- Functors: basic and flavours
-----

{- | Prototypal functor from Category @c@ to Category @d@

Functors form a Category over Categories, hence are also an instance
of Category.
-}
data FunctorArrow c d where
    FunctorArrow :: (Category c, Category d) => {
        pre     :: c a' (d a b) -> d a' b,
        post    :: d a (c a b') -> c a b'
    } -> FunctorArrow c d

{- | Prototypal monoidal functor from Category @c@ to Category @d@
-}
data MonoidalFunctor p c d where
    MonoidalFunctor :: {
        prem    :: (),
        postm   :: ()
    } -> MonoidalFunctor p c d

data CoMonoidalFunctor s c d where
    CoMonoidalFunctor :: {
        pres    :: (),
        posts   :: ()
    } -> CoMonoidalFunctor s c d

{- | #Example# == Example
This example shows that the Functor datatype can encode the @map@ function
for Lists. An endo-functor in (->), i.e., a record of
@ArrowFunctor (->) (->)@ has members

> pre   :: (a' -> (a -> b)) -> (a' -> b)
> post  :: (a -> (a -> b')) -> (a -> b')

Now suppose that primed types are the corresponding List type, such that 
@a' ~ List a@ and @b' ~ List b@. The record therefore has components

> pre   :: ([a] -> (a -> b)) -> ([a] -> b)
> post  :: (a -> (a -> [b])) -> (a -> [b])

-}