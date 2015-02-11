--------------------------------------------------------------------------------
-- |
-- Module       :   Access.Core
-- Copyright    :   (c) Aaron Stevens, 2014
-- License      :   GPL2
--
-- Maintainer   :   bheklilr2@gmail.com
--------------------------------------------------------------------------------

module Access.Core
    ( Access
    ) where


import           Control.Applicative (Applicative)
import           Control.Monad.Fix   (MonadFix)
import           Data.Typeable       (Typeable)

-- | The 'Access' type class.  It belongs to several of the same type classes
-- as 'IO'.  Notably, it is a 'Monad', a 'Functor', and an 'Applicative'.
class ( Monad io
      , Functor io
      , Typeable io
      , MonadFix io
      , Applicative io) => Access io where


instance Access IO where
