{-# LANGUAGE
		NoImplicitPrelude
  #-}

module Alt.Prelude (
		($),
		-- re-exports
		Prelude.undefined,
		Prelude.error
	) where

import qualified Prelude (undefined, error)

infixr 0 $

($) :: (a -> b) -> a -> b
($) f = \x -> f x

