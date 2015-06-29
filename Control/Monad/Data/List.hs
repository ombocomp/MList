{-# LANGUAGE LambdaCase #-}

module Control.Monad.Structure.List where

import Control.Monad.Catch
import Control.Monad.Data.Foldable
import Control.Monad.Data.List.Internal
import Control.Monad.Data.Types (MList(..), BiCons(..))
import qualified Data.List.Safe as LS

-- |Tests whether an MList is empty.
nullML :: Functor m => MList m a -> m Bool
nullML (ML xs) = fmap (\case{Nil -> True; _ -> False}) xs

-- |Returns the length of an MList.
lengthML :: (Monad m, Integral i) => MList m a -> m i
lengthML = foldrM (\_ x -> (x+1)) 0

-- |Returns the head of an MList.
headML :: (Monad m, MonadThrow r) => MList m a -> m (r a)
headML = maybeHeadML return (throwM LS.EmptyListException)

-- |Returns the tail of an MList.
tailML :: (Monad m, MonadThrow r) => MList m a -> m (r (MList m a))
tailML = maybeTailML return (throwM LS.EmptyListException)

-- |Returns an MList without its last element.
initML :: (Monad m, MonadThrow r) => MList m a -> m (r (MList m a))
initML = maybeInitML return (throwM LS.EmptyListException)

-- |Returns the last element of an MList.
lastML :: (Monad m, MonadThrow r) => MList m a -> m (r a)
lastML = maybeLastML return (throwM LS.EmptyListException)

{-

-- |Folds an MList from the left.
foldlML :: Monad m => (a -> b -> m a) -> a -> MList m b -> m a
foldlML _ acc MNil = return acc
foldlML f acc (x :# xs) = do acc' <- f acc x
                             xs' <- xs
                             foldlML f acc' xs'

-- |Folds an MList from the right.
foldrML :: Monad m => (a -> b -> m b) -> b -> MList m a -> m b
foldrML _ acc MNil = return acc
foldrML f acc (x :# xs) = do y <- (f x acc)
                             xs' <- xs
                             foldrML f y xs'

-- |Unfolds an MList.
unfoldML :: Monad m => (b -> m (Maybe (a,b))) -> b -> m (MList m a)
unfoldML f acc = do v <- f acc
                    return (case v of Nothing       -> MNil
                                      Just (x,acc') -> x :# unfoldML f acc')

-- |Takes n elements from the beginning of an MList.
takeML :: Monad m => Int -> MList m a -> MList m a
takeML n _ | n <= 0 = MNil
takeML _ MNil = MNil
takeML i (x :# xs) = x :# fmap (takeML (i-1)) xs

-- |Drops n elements from the beginning of an MList
dropML :: Monad m => Int -> MList m a -> m (MList m a)
dropML n xs | n <= 0 = return xs
dropML _ MNil = return MNil
dropML i (_ :# xs) = xs >>= dropML (i-1)

-- |Takes elements of an MList as long as a predicate is fulfilled.
takeWhileML :: Monad m => (a -> Bool) -> MList m a -> MList m a
takeWhileML _ MNil = MNil
takeWhileML f (x :# xs) | f x       = x :# fmap (takeWhileML f) xs
                        | otherwise = MNil

-- |Drops elements from an MList as long as a predicate is fulfilled.
dropWhileML :: Monad m => (a -> Bool) -> MList m a -> m (MList m a)
dropWhileML _ MNil = return MNil
dropWhileML f (x :# xs) | f x       = xs >>= dropWhileML f
                        | otherwise = xs

-- |Reverses an MList.
reverseML :: Monad m => MList m a -> m (MList m a)
reverseML = reverse' MNil
  where reverse' acc MNil = return acc
        reverse' acc (x :# xs) = xs >>= reverse' (x :# return acc)

-}
