{-# LANGUAGE
        NoImplicitPrelude,
        GADTs,
        StandaloneDeriving
  #-}

module Alt.Object.Identity (
        Id(..)
    ) where


import Alt.Prelude (Eq, Show, Typeable)

{- | Identity type-level function, intended to encapsulate objects. Contents must 
be an instance of 'Typeable' and 'Eq'.
-}
data Id a where
    Id      :: (Typeable a, Eq a) => a -> Id a

deriving instance Show a => Show (Id a)