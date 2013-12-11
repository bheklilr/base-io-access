module Access.Core
    ( Access
    ) where


import Data.Typeable.Internal (Typeable1)
import Control.Applicative    (Applicative)
import Control.Monad.Fix      (MonadFix)

class ( Monad io
      , Functor io
      , Typeable1 io
      , MonadFix io
      , Applicative io) => Access io where


instance Access IO where
