{-# LANGUAGE
    NoImplicitPrelude
  #-}

{-|
Module:             Alt.Arrow.Core
Description:        Basic Arrows: Identity, Error, Reader, Writer, State, RWS
Copyright:          (c) 2015 Nicolas Godbout
Licence:            BSD-3
Stability:          experimental

This module re-exports a select set of Arrows.
-}

module Alt.Arrow.Core (
        IdentityArrow(..),
        ErrorArrow(..),
        ReaderArrow(..),
        WriterArrow(..),
        StateArrow(..),
        RWSArrow(..)
    ) where

-- alt-base modules
import Alt.Category

-- base modules
import Data.Either (Either(..))

-- alt-base Prelude
import Alt.Prelude


newtype IdentityArrow f a b = IdentityArrow {
    identityArrow :: f a b
} deriving (Category, CatHaskell)


newtype ErrorArrow e f a b = ErrorArrow {
    errorArrow :: f a (Either e b)
}

newtype ReaderArrow r f a b = ReaderArrow {
    readerArrow :: f (a,r) b
}

newtype WriterArrow w f a b = WriterArrow {
    writerArrow :: f a (b,w)
}

newtype StateArrow s f a b = StateArrow {
    stateArrow :: f (a,s) (b,s)
}

newtype RWSArrow r w s f a b = RWSArrow {
    rwsArrow :: f (a,s,r) (b,s,w)
}


newtype STArrow f a b = STArrow {
    stArrow :: forall s. (a, ST s) -> (b, ST s)
}
-- or something like that...

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
