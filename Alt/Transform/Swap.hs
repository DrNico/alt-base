{-# LANGUAGE
		NoImplicitPrelude,
		MultiParamTypeClasses
	#-}


{- |
Module          : Alt.Transform.Swap
Description     : Swap two unary type functions
Copyright       : (c) Nicolas Godbout, 2015
License         : BSD-3
Maintainer      : nicolas.godbout@gmail.com
-}
module Alt.Transform.Swap (
		Swap(..)
	) where

-- base modules
import Data.Maybe (Maybe(..))
import Control.Monad (Functor(..))


{- | Class of pairs of type functions that are naturally swappable.
-}
class Swap g f where
	swap :: g (f a) -> f (g a)

{- | @'Just' [x]@ iff no list member is 'Nothing'.
-}
instance Swap [] Maybe where
	swap []             = Just []
	swap (Nothing : xs) = Nothing
	swap (Just x : xs)  = fmap ((:) x) (swap xs)
