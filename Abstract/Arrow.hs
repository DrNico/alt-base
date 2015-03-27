{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module          : Abstract.Arrow
Description     : Arrow Categories
Copyright       : (c) Nicolas Godbout, 2015
License         : BSD-3
Maintainer      : nicolas.godbout@gmail.com
-}
module Abstract.Arrow (
		Arrow0(..),
		Arrow1(..),
		Arrow2(..)
	) where

-- alt-base modules
import Abstract.Category

-- base modules
import Control.Applicative (Applicative(..))
import Data.Either (Either(..))

-- needed from Prelude
import Prelude (($), const)

class (Category f) => Arrow f where
	arr 		:: (a -> b) -> f a b


-- t is a nullary transformer, hence Set?
class (Category f) => Arrow0 t f where
	pull0       :: (t -> b) -> f t b 	-- const b
	push0		:: t -> f a t
	-- if t is a Product, then push0 == const One
	-- if t is a Sum, then push0 == Zero, i.e. 'fail'

class (Category f) => Arrow1 t f where
	pull1		:: (t a -> b) -> f (t a) b
	push1		:: t (a -> b) -> f a (t b)
	-- if t == Id then pull1 == push1 ~ arr

class (Category f) => Arrow2 t f where
	pull2		:: (t a b -> c) -> f (t a b) c
	push2       :: t (a -> b) (a -> c) -> f a (t b c)
	-- if t is a Product, then this Category is Monoidal
	-- if t is a Sum, then this Category is CoMonoidal

data Id a = Id a

instance Arrow0 t (->) where
	pull0 = id
	push0 = const

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





