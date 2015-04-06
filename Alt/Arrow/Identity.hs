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
import Alt.Abstract.Arrow
import Alt.Abstract.Category

-- alt-base Prelude
import Alt.Prelude

{- | Identity arrow, merely encapsulating another arrow.
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

deriving instance Haskell f     => Haskell (IdentityArrow f)
deriving instance Arrow_ f      => Arrow_ (IdentityArrow f)
deriving instance Arrow0 t f    => Arrow0 t (IdentityArrow f)
deriving instance Arrow1 t f    => Arrow1 t (IdentityArrow f)
-- deriving instance Arrow2 t f    => Arrow2 t (IdentityArrow f)
-- deriving instance Arrow3 t f    => Arrow3 t (IdentityArrow f)

-- instance Arrow f => ArrowTrans IdentityArrow f where
--     liftA = IdentityArrow

