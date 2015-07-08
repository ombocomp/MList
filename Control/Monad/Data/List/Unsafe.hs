-- |Contains unsafe partial functions on MLists.
--  These are provided for convenience;
--  the safe versions in "Control.Monad.Data.List" are recommended.
module Control.Monad.Data.List.Unsafe where

import Control.Monad.Data.List.Internal
import Control.Monad.Data.Types (MList(..), BiCons(..))

-- |Returns the head of an MList.
headML :: Functor m => MList m a -> m a
headML = maybeHeadML (error "headML: empty list!") id

tailML :: Functor m => MList m a -> m (MList m a)
tailML = maybeTailML id (error "tailML: empty MList!")

initML :: Applicative m => MList m a -> m (MList m a)
initML = maybeInitML id (error "initML: empty MList!")

lastML :: Monad m => MList m a -> m a
lastML = maybeLastML id (error "lastML: empty MList!")
