{-# LANGUAGE RankNTypes #-}

{-|
Module:             Abstract.Yoneda
Description:        Yoneda representation of arrows
Copyright:          (c) 2015 Nicolas Godbout
Licence:            BSD-3
Stability:          experimental
Portability:        not portable (RankNTypes)

The Yoneda representation is essentially the continuation-based version of
the modeled computation.
-}
module Abstract.Yoneda (
		-- * Yoneda Class
		Yoneda(..),
		-- * Yoneda Instances
		IdentityYoneda(..),
		ErrorYoneda(..),
		ReaderYoneda(..),
		WriterYoneda(..),
		StateYoneda(..)
		-- * Turing machine in Yoneda representation
		-- TuringYoneda(..)
	) where

import Abstract.Category

import Control.Category
import Data.Monoid (Monoid(..))

import Prelude (($), Either(..))

-----
-- Specialized Categories
-- ## move to another module
-----

class Category f => Monoidal f where
	pull	:: (a -> b -> f () c) -> f (a,b) c
	push	:: f a b -> f a c -> f a (b,c)
	const   :: a -> f z a

class Category f => CoMonoidal f where
	copull  :: (Either a b -> f () c) -> f (Either a b) c
	copush  :: (Either (f a b) (f a c)) -> f a (Either b c)


class Yoneda y where
	liftY   :: f a b -> (y f) a b
	unliftY :: (y f) a b -> f a b

-- | Dual of @f a b@
newtype IdentityYoneda f a b = IdentityYoneda {
	unliftYoneda :: forall z. Category f => f b z -> f a z
}

-- | Dual of @f a (Either e b)@
newtype ErrorYoneda e f a b = ErrorYoneda {
	unliftErrorYoneda :: CoMonoidal f => forall z. f e z -> f b z -> f a z
}

-- | Dual of @f (a,r) b@
newtype ReaderYoneda r f a b = ReaderYoneda {
	unliftReaderYoneda :: Category f => forall z. r -> f b z -> f a z
}

-- | Dual of @f a (b,w)@
newtype WriterYoneda w f a b = WriterYoneda {
	unliftWriterYoneda :: (Monoidal f, Monoid w) => forall z. f (b,w) z -> f a z
}

-- | Dual of @f (a,s) (b,s)
newtype StateYoneda s f a b = StateYoneda {
	unliftStateYoneda :: Monoidal f => forall z. f (b,s) z -> f (a,s) z
}

-- ProductYoneda	(Product a, Product b) => f a b
-- SumYoneda		(Sum a, Sum b) => f a b
class Product (p :: 'HList -> *) where

class Sum (s :: 'HList -> *) where


-----
-- Instances
-----

instance Category c => Category (IdentityYoneda c) where
	id = IdentityYoneda id
	g . f = IdentityYoneda $
				unliftYoneda f . unliftYoneda g

instance Yoneda IdentityYoneda where
	liftY f = IdentityYoneda $ \h -> h . f
	-- unliftY f = unliftYoneda f id
	-- ^ ## does not compile: add type annotations

instance Category c => Category (ErrorYoneda e c) where
	id = ErrorYoneda $ \_ -> id
	g . f = ErrorYoneda $ \e -> unliftErrorYoneda f e . unliftErrorYoneda g e

instance Yoneda (ErrorYoneda e) where
	liftY f = ErrorYoneda $ \_ h -> h . f

instance Yoneda (ReaderYoneda r) where
	liftY f = ReaderYoneda $ \_ h -> h . f

instance Yoneda (WriterYoneda w) where
	liftY f = WriterYoneda $ \h -> h . push id (const mempty) . f
