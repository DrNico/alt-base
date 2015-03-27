{-# LANGUAGE
    NoImplicitPrelude
  #-}

{-|
Module:             Arrow.Core
Description:        Basic Arrows: Identity, Error, Reader, Writer, State, RWS
Copyright:          (c) 2015 Nicolas Godbout
Licence:            BSD-3
Stability:          experimental

This module re-exports a select set of Arrows.
-}

module Arrow.Core (
        IdentityArrow(..),
        ErrorArrow(..),
        ReaderArrow(..),
        WriterArrow(..),
        StateArrow(..),
        RWSArrow(..)
    ) where

-- alt-base modules
import Abstract.Category

-- base modules
import Data.Either (Either(..))

-- begin µPrelude
($) :: (a -> b) -> a -> b
($) f = \x -> f x
infixr 0 $
-- end µPrelude

newtype IdentityArrow f a b = IdentityArrow {
    runIdentity :: f a b
}

newtype ErrorArrow e f a b = ErrorArrow {
    runError :: f a (Either e b)
}

newtype ReaderArrow r f a b = ReaderArrow {
    runReader :: f (a,r) b
}

newtype WriterArrow w f a b = WriterArrow {
    runWriter :: f a (b,w)
}

newtype StateArrow s f a b = StateArrow {
    runState :: f (a,s) (b,s)
}

newtype RWSArrow r w s f a b = RWSArrow {
    runRWS :: f (a,s,r) (b,s,w)
}

-----
-- Instances
-----

-- Category
-- Arrow
-- ArrowProduct
-- ArrowSum
-- ArrowApply
-- ArrowLoop
-- ArrowTrans
