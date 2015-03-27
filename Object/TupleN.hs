{-# LANGUAGE
	NoImplicitPrelude,
	GADTs, PolyKinds, TypeFamilies
  #-}

{-|
Module          : Object.TupleN
Description     : Tuples of arbitrary length
Copyright       : (c) Nicolas Godbout, 2015
License         : BSD-3
Maintainer      : nicolas.godbout@gmail.com
-}
module Object.TupleN (
		TupleN(..)
	) where

-- alt-base modules
import Abstract.Category (Product(..))


data TupleN a r where
	NilTN		:: TupleN () ()
	ConsTN		:: a -> TupleN b r -> TupleN a (TupleN b r)


instance Product TupleN where
	type One TupleN = TupleN () ()
	
	fst NilTN			= ()
	fst (ConsTN x _)	= x

	snd NilTN			= ()
	snd (ConsTN _ r)	= r

-- we should also introduce the concept of Section, a view of the head
-- seen as a section of 'drop'
