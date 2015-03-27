{-# LANGUAGE
    NoImplicitPrelude,
    RankNTypes,
    GADTs
  #-}

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

import Data.Monoid (Monoid(..))

import Prelude (($), Either(..))

-----
-- Specialized Categories
-- ## move to another module
-----

class Yoneda y where
    liftY   :: forall f a b. Category f => f a b -> (forall z. (y z f) a b)
    unliftY :: forall f a b. (forall z. (y z f) a b) -> f a b


{- | Internal Yoneda representation of an arrow in an arbitrary category.
-}
newtype Yoneda z f a b = Yoneda {
    f (f b z) (f a z)
}

-- | Dual of @f a b@
newtype IdentityYoneda z f a b = IdentityYoneda {
    unliftYoneda :: f b z -> f a z
}

-- | Dual of @f a (Either e b)@
newtype ErrorYoneda e z f a b = ErrorYoneda {
    unliftErrorYoneda :: f e z -> f b z -> f a z
}

-- | Dual of @f (a,r) b@
newtype ReaderYoneda r z f a b = ReaderYoneda {
    unliftReaderYoneda :: f b z -> f (a,r) z
}

-- | Dual of @f a (b,w)@
newtype WriterYoneda w z f a b = WriterYoneda {
    unliftWriterYoneda :: f (b,w) z -> f a z
}

-- | Dual of @f (a,s) (b,s)
newtype StateYoneda s z f a b = StateYoneda {
    unliftStateYoneda :: f (b,s) z -> f (a,s) z
}

{-  TODO
investigate the following scheme

data family Yoneda a b
data instance Yoneda f (Id a) (Id b)        = Yoneda {}
data instance Yoneda f (Id a) (Either a b)  = ErrorYoneda {}
data instance Yoneda f (a,r) (Id a)         = ReaderYoneda {}
data instance Yoneda f (Id a) (a,w)         = WriterYoneda {}
data instance Yoneda f (a,s) (a,s)          = StateYoneda {}
-}

-- ProductYoneda    (Product a, Product b) => f a b
-- SumYoneda        (Sum a, Sum b) => f a b

-- class Product (p :: 'HList -> *) where
-- class Sum (s :: 'HList -> *) where


-----
-- Instances
-----

instance Category c => Category (IdentityYoneda z c) where
    idC _ = IdentityYoneda $ id
    g . f = IdentityYoneda $
                unliftYoneda f . unliftYoneda g

instance Yoneda IdentityYoneda where
    liftY f = IdentityYoneda $ \h -> h . f
    -- unliftY f = unliftYoneda f id
    -- ^ ## does not compile: add type annotations
{-
instance Category c => Category (ErrorYoneda e c) where
    idC proxy = ErrorYoneda $ \_ -> idC proxy
    g . f = ErrorYoneda $ \e -> unliftErrorYoneda f e . unliftErrorYoneda g e

instance Yoneda (ErrorYoneda e) where
    liftY f = ErrorYoneda $ \_ h -> h . f

instance Yoneda (ReaderYoneda r) where
    liftY f = ReaderYoneda $ \_ h -> h . f

instance Yoneda (WriterYoneda w) where
    liftY f = WriterYoneda $ \h -> h . push id (const mempty) . f
-}
