{-# LANGUAGE
		NoImplicitPrelude
  #-}

module Alt.Prelude (
		($),
		Prelude.undefined
	) where

import qualified Prelude (undefined, error)

infixr 0 $

($) :: (a -> b) -> a -> b
($) f = \x -> f x

