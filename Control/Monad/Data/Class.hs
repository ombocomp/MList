{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Data.Class where

-- |Monadic data structures that have a non-monadic counterpart that
--  permits bi-directional conversaion
class MonadicStructure s where
   type Target s :: * -> *
   -- |Converts a non-monadic structure into the monadic one.
   toMonadic :: Applicative m => Target s a -> s m a
   -- |Converts a monadic structure into the non-monadic one.
   fromMonadic :: Monad m => s m a -> m (Target s a)
