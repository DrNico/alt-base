{-# LANGUAGE
        NoImplicitPrelude,
        GADTs,
        PolyKinds,
        TypeFamilies,
        TypeOperators,
        ScopedTypeVariables
  #-}

{-|
Module          : Alt.Abstract.Category
Description     : Class of Categories
Copyright       : (c) Nicolas Godbout, 2015
License         : BSD-3
Maintainer      : nicolas.godbout@gmail.com

This module is the foundation of the alt-base package and is imported by
most other modules.
-}
module Alt.Abstract.Category (
        -- * Category class
        Category(..),
        (>>>), (<<<),
        -- * Haskell category
        Haskell(..)
    ) where

-- alt-base modules
import Alt.Abstract.Equivalence
import Alt.Object.Identity

-- base modules
import Data.Bool (Bool(..))
import Data.Eq (Eq(..))
import Data.Maybe (Maybe(..))
import Data.Typeable


infixr 9 .
infixr 1 <<<
infixr 1 >>>

-----
-- Comment:
--   cute, but we really need to distinguish between small and large categories
----

{- | General class of Categories, with two associated types: a type of Objects
and an Equivalence of Objects.

Instances must satisfy the following laws:

prop> f . (idC (source f)) == f
prop> (idC (target g)) . g == g
prop> h . (g . f) == (h . g) . f
prop> g . f == bottom "if and only if" idS g `idEq` idT f

-}
class Category hom where
    type EqC hom a b :: *
    type Obj hom a   :: *

    -- | Get the source of the morphism.
    source  :: hom a b -> Obj hom a

    -- | Get the target of the morphism.
    target  :: hom a b -> Obj hom b

    -- | Build an identity morphism over the given object.
    idC     :: Obj hom a -> hom a a

    -- | Composition with a witness obtained from the Equivalence class.
    dotW    :: EqC hom b b'
            -> hom b' c -> hom a b
            -> hom a c


-- ; Composition of morphisms within an ambiant category that can
-- handle errors in case of equivalence violation. Typical use is for
-- dynamic type-checking in interpreters, domain-specific languages, etc.
-- dot :: (ArrowError CatError amb, Equivalence (EqC hom))
--     => amb (hom b' c, hom a b) (hom a c)
-- ## break circular reference from ErrorArrow

{- | Category of function-like morphisms in Haskell.

Instances of this category have the property that equality of type is
/sufficient/ to ensure composability. The Haskell type system can always
correctly determine the correct composition.

/Note/: This definition is identical but distinct from the standard "Control.Category"
module, mostly to avoid utter confusion with the 'Category' class of this module, in some
part to impose different simplification rules.
Contact maintainers if you need this class to unify with the standard.
-}
class Haskell hom where
    id      :: hom a a
    (.)     :: hom b c -> hom a b
            -> hom a c

(<<<) :: (Haskell hom)
      => hom b c -> hom a b
      -> hom a c
{-# INLINE (<<<) #-}
(<<<) = (.)

(>>>) :: (Haskell hom)
      => hom a b -> hom b c
      -> hom a c
{-# INLINABLE (>>>) #-}
f >>> g = g . f



instance Category (->) where
    type EqC (->) a b = a :~: b
    type Obj (->) a   = Proxy a

    {-# INLINE source #-}
    source (f :: a -> b) = Proxy :: Proxy a
    {-# INLINE target #-}
    target (f :: a -> b) = Proxy :: Proxy b

    {-# INLINE idC #-}
    idC Proxy = \x -> x

    {-# INLINE dotW #-}
    dotW Refl = \g f -> \x -> g (f x)

instance Haskell (->) where
    {-# INLINE id #-}
    id = \x -> x

    {-# INLINE (.) #-}
    g . f = \x -> g (f x)


instance Category (,) where
    type EqC (,) a b = a :=: b
    type Obj (,) a   = a

    {-# INLINE source #-}
    source (x,_) = x
    {-# INLINE target #-}
    target (_,y) = y

    idC x = (x,x)

    {-# INLINE dotW #-}
    dotW (Equal _) = \(_,y) (x,_) -> (x,y)
