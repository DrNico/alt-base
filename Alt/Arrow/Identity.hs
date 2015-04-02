{-# LANGUAGE
    NoImplicitPrelude,
    TypeFamilies,
    FlexibleInstances,
    StandaloneDeriving,
    MultiParamTypeClasses,
    GeneralizedNewtypeDeriving
  #-}

{-|
Module:             Alt.Arrow.Identity
Description:        Arrow encapsulating a pure Haskell function
Copyright:          (c) 2015 Nicolas Godbout
Licence:            BSD-3
Stability:          experimental
-}
module Alt.Arrow.Identity (
        IdentityArrow(..)
    ) where

-- alt-base modules
import Alt.Arrow
import Alt.Category

-- alt-base Prelude
import Alt.Prelude

{- | Identity arrow, merely encapsulating a pure Haskell function.
-}
newtype IdentityArrow f a b = IdentityArrow {
    identityArrow :: f a b
}

instance Category f => Category (IdentityArrow f) where
    type EqC (IdentityArrow f) a b = EqC f a b
    type Obj (IdentityArrow f) a   = Obj f a

    source (IdentityArrow f) = source f
    target (IdentityArrow f) = target f

    idC o = IdentityArrow (idC o)
    dotW refl = dot
        where
        dot (IdentityArrow g) (IdentityArrow f) =
            IdentityArrow (dotW refl g f)

deriving instance CatHaskell f => CatHaskell (IdentityArrow f)
deriving instance Arrow f => Arrow (IdentityArrow f)
deriving instance ArrowProd f => ArrowProd (IdentityArrow f)
deriving instance ArrowSum f => ArrowSum (IdentityArrow f)
-- deriving instance ArrowApply f => ArrowApply (IdentityArrow f)
deriving instance ArrowLoop f => ArrowLoop (IdentityArrow f)

instance Arrow f => ArrowTrans IdentityArrow f where
    liftA = IdentityArrow

