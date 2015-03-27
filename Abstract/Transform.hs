{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module:             Abstract.Transform
Description:        Class of Object transformers
Copyright:          (c) 2015 Nicolas Godbout
Licence:            BSD-3
Stability:          experimental
Portability:        portable

-}
module Abstract.Transform where

-- alt-base modules
-- import Abstract.Arrow
import Abstract.Category


{- | Class of natural transformations from Arrow to Arrow.

A 'Transform' instance tranforms an object into another object, coming
with a functor which transforms functions into functions. Examples of
'Transform' instances are 'List' and 'Maybe'.

In the Haskell standard, this is named the @Functor@ class. It is here
extended as an endofunctor in any Category.
-}
class Transform t where
	tmap		:: Category f => f a b -> f (t a) (t b)

