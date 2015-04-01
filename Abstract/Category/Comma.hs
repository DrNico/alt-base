

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



class (LargeCategory f) => Arrow2 t f where
    pull2       :: (t a b -> c) -> f (t a b) c
    push2       :: t (a -> b) (a -> c) -> f a (t b c)

data Comma f g = Comma {
	cleft 		:: ()
}


-- write two functions proving the existence of a commuting square
-- see Wikipeda on Comma Category
data Comma (f (S a) (T b))
		   (g a a') 
		   (h b b')  
			= Comma {
	sFunctor 	:: 
	tFunctor 	:: 
}

f  : C S(a) T(b)
f' : C S(a') T(b')
g : A a a'
h : B b b'
S(g) . f' == f . T(h)
