{-# LANGUAGE
    NoImplicitPrelude,
    TypeFamilies,
    TypeOperators,
    FlexibleInstances,
    StandaloneDeriving,
    ScopedTypeVariables,
    MultiParamTypeClasses,
    GeneralizedNewtypeDeriving
  #-}

{-|
Module:             Alt.Arrow.Kleisli
Description:        Arrow encapsulating a Haskell-style Monad
Copyright:          (c) 2015 Nicolas Godbout
Licence:            BSD-3
Stability:          experimental
-}
module Alt.Arrow.Kleisli (
        KleisliArrow(..)
    ) where

-- alt-base modules
import Alt.Abstract.Arrow
import Alt.Abstract.Category

-- base modules
import Control.Monad (Monad(..), join)
import Data.Typeable ((:~:)(..), Proxy(..))

-- alt-base Prelude
import Alt.Prelude

{- | Kleisli arrow, encapsulating a Haskell-type Monad
-}
newtype KleisliArrow m f a b = KleisliArrow {
    kleisliArrow :: f a (m b)
}

instance (Monad m) => Haskell (KleisliArrow m (->)) where
    id = KleisliArrow return

    (KleisliArrow g) . (KleisliArrow f) =
        KleisliArrow $ \x -> f x >>= g
            -- arr join .  . f

    {-# INLINE id #-}
    {-# INLINE (.) #-}


instance (Monad m) => Category (KleisliArrow m (->)) where
    type EqC (KleisliArrow m (->)) a b = a :~: b
    type Obj (KleisliArrow m (->)) a   = Proxy a

    source (KleisliArrow (f :: a -> m b)) = Proxy :: Proxy a
    target (KleisliArrow (f :: a -> m b)) = Proxy :: Proxy b

    idC Proxy = id
    dotW Refl = (.)

    {-# INLINE source #-}
    {-# INLINE target #-}
    {-# INLINE idC #-}
    {-# INLINE dotW #-}


{-
deriving instance CatHaskell f => CatHaskell (IdentityArrow f)
deriving instance Arrow f => Arrow (IdentityArrow f)
deriving instance ArrowProd f => ArrowProd (IdentityArrow f)
deriving instance ArrowSum f => ArrowSum (IdentityArrow f)
-- deriving instance ArrowApply f => ArrowApply (IdentityArrow f)
deriving instance ArrowLoop f => ArrowLoop (IdentityArrow f)
-}

-- instance (Arrow f, CatHaskell f, Monad m) => ArrowTrans (KleisliArrow m) f where
--     liftA f = KleisliArrow $ arr return . f

