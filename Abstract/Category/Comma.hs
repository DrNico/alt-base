

{-
Module          : Abstract.Category.Comma
Description     : Comma Categories
Copyright       : (c) Nicolas Godbout, 2015
License         : BSD-3
Maintainer      : nicolas.godbout@gmail.com

Comma Categories give rise to Arrows, Slices and CoSlices, and most importantly
Functors.
-}
module Abstract.Category.Comma (
	) where



-- write two functions proving the existence of a commuting square
-- see Wikipeda on Comma Category
data CommaWitness f s t = CommaWitness {
	sFunctor 	:: ,
	tFunctor 	:: 
}
