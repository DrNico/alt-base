{-# LANGUAGE
        NoImplicitPrelude,
        MultiParamTypeClasses,
        FlexibleInstances
  #-}


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
import Abstract.Arrow
import Abstract.Category

-- µPrelude
($) :: (a -> b) -> a -> b
($) f = \x -> f x
infixr 0 $
-- µPrelude


{- | Class of natural transformations from Arrow to Arrow.

A 'Transform' instance tranforms an object into another object, coming
with a functor which transforms functions into functions. Examples of
'Transform' instances are 'List' and 'Maybe'.

In the Haskell standard, this is named the @Functor@ class. It is here
extended as an endofunctor in any Category.
-}
class Transform u v where
    tmap        :: Arrow f => f u v

class Product u v where
    pmap        :: ArrowProd f => f (u,v) w -> f u (v,w) -- figure this out


instance Transform a [a] where
    tmap = arr $ \x -> const [x]

