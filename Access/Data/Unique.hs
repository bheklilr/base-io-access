module Access.Data.Unique
    ( module Data.Unique

    , UniqueAccess(..)
    ) where


import Data.Unique

import Access.Core


class Access io => UniqueAccess io where
    newUnique' :: io Unique


instance UniqueAccess IO where
    newUnique' = newUnique
