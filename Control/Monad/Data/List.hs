{-# LANGUAGE LambdaCase #-}

module Control.Monad.Structure.List (
    module Control.Monad.Data.Foldable,
    module Control.Monad.Data.Types,
    -- * List functions
    nullML,
    lengthML,
    headML,
    tailML,
    initML,
    lastML,
    takeML,
    takeWhileML,
    dropML,
    dropWhileML,
    ) where

import Control.Monad.Catch
import Control.Monad.Data.Foldable
import Control.Monad.Data.List.Internal
import Control.Monad.Data.Types (MList(..), BiCons(..), bc)
import qualified Data.List.Safe as LS

-- |Tests whether an MList is empty.
nullML :: Functor m => MList m a -> m Bool
nullML (ML xs) = fmap (\case{Nil -> True; _ -> False}) xs

-- |Returns the length of an MList.
lengthML :: (Monad m, Integral i) => MList m a -> m i
lengthML = foldrM (\_ x -> (x+1)) 0

-- |Returns the head of an MList.
--headML :: (Monad m, MonadThrow r) => MList m a -> m (r a)
--headML = maybeHeadML return (throwM LS.EmptyListException)

headML :: Functor m => MList m a -> m a
headML (ML xs) = fmap (bc (error "headML: empty list!") $ \h _ -> h) xs

-- |Returns the tail of an MList.
--tailML :: (Monad m, MonadThrow r) => MList m a -> m (r (MList m a))
--tailML = maybeTailML return (throwM LS.EmptyListException)

tailML :: (Monad m) => MList m a -> MList m a
tailML (ML xs) = ML (xs >>= bc (error "tailML: empty list!") (\_ t -> runML t))

-- |Returns an MList without its last element.
initML :: (Monad m, MonadThrow r) => MList m a -> m (r (MList m a))
initML = maybeInitML return (throwM LS.EmptyListException)

-- |Returns the last element of an MList.
lastML :: (Monad m, MonadThrow r) => MList m a -> m (r a)
lastML = maybeLastML return (throwM LS.EmptyListException)

takeWhileML :: Functor m => (a -> Bool) -> MList m a -> MList m a
takeWhileML f (ML xs) = ML $
   fmap (bc Nil $ \h t -> if f h then h :# takeWhileML f t else Nil) xs

takeML :: (Functor m, Integral a) => a -> MList m a -> MList m a
takeML 0 xs = xs
takeML n (ML xs) = ML $ fmap (bc Nil $ \h t -> h :# takeML (n-1) t) xs

dropML :: (Monad m, Integral a) => a -> MList m a -> MList m a
dropML 0 xs = xs
dropML n (ML xs) = ML $ xs >>= bc (return Nil) (\_ t -> runML $ dropML (n-1) t)

dropWhileML :: Monad m => (a -> Bool) -> MList m a -> MList m a
dropWhileML f (ML xs) = ML $ xs >>= bc (return Nil) (\h t -> if f h then runML (dropWhileML f t) else return (h :# t))


{-

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
