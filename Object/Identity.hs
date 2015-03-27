{-# LANGUAGE
	PolyKinds
  #-}

module Object.Identity (
		Id(..)
	) where

{- | Identity type-level function
-}
data Id a = Id a
