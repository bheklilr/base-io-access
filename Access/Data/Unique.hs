module Access.Data.Unique
    ( module Data.Unique

    , UniqueAccess(..)
    ) where


import Data.Unique


class Monad io => UniqueAccess io where
    newUnique' :: io Unique


instance UniqueAccess IO where
    newUnique' = newUnique
