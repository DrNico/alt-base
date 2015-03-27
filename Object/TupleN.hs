{-# LANGUAGE
	NoImplicitPrelude,
	GADTs, PolyKinds, TypeFamilies
  #-}

module Object.TupleN (
		TupleN(..)
	) where

-- alt-base modules
import Abstract.Category (Product(..), CoProduct(..))


data TupleN a r where
	NilTN		:: TupleN () ()
	ConsTN		:: a -> TupleN b r -> TupleN a (TupleN b r)


instance Product TupleN where
	type One TupleN = TupleN () ()
	
	fst NilTN			= ()
	fst (ConsTN x _)	= x

	snd NilTN			= ()
	snd (ConsTN _ r)	= r

