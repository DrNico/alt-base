{-# LANGUAGE
		NoImplicitPrelude
  #-}

{- |
Module          : Alt.Prelude
Description     : Small Prelude to use with other modules.
Copyright       : (c) Nicolas Godbout, 2015
License         : BSD-3
Maintainer      : nicolas.godbout@gmail.com

Small Prelude for use with other modules.

Remark: Please use this prelude's '$' operator. The standard Prelude version does
not compose well with this package's combinators.
-}
module Alt.Prelude (
		($),
		-- re-exports
		Prelude.undefined,
		Prelude.error
	) where

import qualified Prelude (undefined, error)

infixr 0 $

{- | Application operator.

> f $ x = f x

Use cases:

  * As a section
	
    	> map (($) f) xs
	
	where this expression produces much cleaner code than both the Prelude
	version of @($)@ and the simpler-looking @map f xs@.
	
  * As a replacement for parentheses
	
    	> f $ g y $ h z == f (g y (h z))
	
    where the inlining strategy for '$' produces exactly the same expression.

-}
($) :: (a -> b) -> a -> b
{-# INLINE ($) #-}
($) f = \x -> f x
