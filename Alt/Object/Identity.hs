{-# LANGUAGE
		PolyKinds
  #-}

module Alt.Object.Identity (
		Id(..)
	) where

{- | Identity type-level function
-}
data Id a = Id a deriving (Eq, Show, Ord)
