{-# LANGUAGE
    NoImplicitPrelude,
    RankNTypes,
    GADTs,
    MultiParamTypeClasses, FunctionalDependencies
  #-}

{-|
Module:             Abstract.Yoneda
Description:        Yoneda representation of Arrows
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
import Abstract.Arrow
import Arrow.Core

import Data.Either (Either(..))
import Data.Monoid (Monoid(..))

-- µPrelude
($) :: (a -> b) -> a -> b
($) f = \x -> f x
infixr 0 $
-- µPrelude

{- | Class of the Yoneda representation of an Arrow transormer.

Instances must satisfy the laws

prop> unliftY . liftY = id
prop> liftY . unliftY = id

which constitute proof that both representations are equivalent. 
The first law will often respect equality “on the nose”, while
the second law will often be an equivalence, i.e., modify the
Yoneda object while preserving its meaning and its effect.
-}
class Yoneda y f where
    -- | Lift a morphism into its Yoneda representation
    liftY   :: forall a b. f a b -> (forall z. (y z) a b)

    -- | Unlift a morphism from its Yoneda representation
    unliftY :: forall a b. (forall z. (y z) a b) -> f a b

{- | Internal Yoneda representation of an Arrow into itself.
-}
newtype EndoYoneda f z a b = EndoYoneda {
    endoYoneda :: f (f b z) (f a z)
}



instance Arrow f => Yoneda (IdentityYoneda f) (IdentityArrow f) where
    liftY (IdentityArrow f)     = IdentityYoneda $ \h -> h . f
    unliftY (IdentityYoneda f)  = IdentityArrow $ f $ idC undefined


-----
-- type instances
-- ## move into Arrow.Yoneda.Core re-exporting modules
-----


-- | Dual of @f a b@
newtype IdentityYoneda f z a b = IdentityYoneda {
    identityYoneda :: f b z -> f a z
}

-- | Dual of @f a (Either e b)@
newtype ErrorYoneda e f z a b = ErrorYoneda {
    errorYoneda :: f e z -> f b z -> f a (Either z z)
}

-- | Dual of @f (a,s) (b,s)
newtype StateYoneda s f z a b = StateYoneda {
    stateYoneda :: f (b,s) z -> f (a,s) (z,z)
}

-- | Dual of @f (a,r) b@
newtype ReaderYoneda r f z a b = ReaderYoneda {
    readerYoneda :: f b z -> f (a,r) z
}

-- | Dual of @f a (b,w)@
newtype WriterYoneda w f z a b = WriterYoneda {
    writerYoneda :: f (b,w) z -> f a z
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
{-
instance Category c => Category (IdentityYoneda z c) where
    idC _ = IdentityYoneda $ id
    g . f = IdentityYoneda $
                unliftYoneda f . unliftYoneda g

instance Yoneda IdentityYoneda where
    liftY f = IdentityYoneda $ \h -> h . f
    -- unliftY f = unliftYoneda f id
    -- ^ ## does not compile: add type annotations

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
