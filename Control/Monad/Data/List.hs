{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Data.List (
   module Control.Monad.Data.Class,
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
   iterateML,
   unfoldML,
   reverseML,
   replicateML,
   repeatML,
   cycleML,
   elemML,
   notElemML,
   zipML,
   zip3ML,
   zip4ML,
   zip5ML,
   zip6ML,
   zip7ML,
   zip8ML,
   zipWithML,
   zipWith3ML,
   zipWith4ML,
   zipWith5ML,
   zipWith6ML,
   zipWith7ML,
   zipWith8ML,
   ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Data.Class
import Control.Monad.Data.Foldable
import Control.Monad.Data.List.Internal
import Control.Monad.Data.Types (MList(..), BiCons(..), bc)
import qualified Data.List.Safe as LS

-- |Tests whether an MList is empty.
nullML :: Functor m => MList m a -> m Bool
nullML (ML xs) = bc True (const . const False) <$> xs

-- |Returns the length of an MList.
lengthML :: (Monad m, Integral i) => MList m a -> m i
lengthML = foldrM (\_ x -> (x+1)) 0

headML :: (Functor m, MonadThrow r) => MList m a -> m (r a)
headML = maybeHeadML (throwM LS.EmptyListException) return

tailML :: (Monad m) => MList m a -> MList m a
tailML (ML xs) = ML $ xs >>= bc (error "tailML: empty list!") (\_ t -> runML t)

initML = undefined
lastML = undefined

-- |Returns an MList without its last element.
--initML :: (Monad m, MonadThrow r) => MList m a -> m (r (MList m a))
--initML = maybeInitML return (throwM LS.EmptyListException)

-- |Returns the last element of an MList.
--lastML :: (Monad m, MonadThrow r) => MList m a -> m (r a)
--lastML = maybeLastML return (throwM LS.EmptyListException)

takeWhileML :: Monad m => (a -> m Bool) -> MList m a -> MList m a
takeWhileML f (ML xs) = ML $ xs >>= bc (return Nil) (\h t -> \case{True -> h :# takeWhileML f t; False -> Nil} <$> f h)

takeML :: (Applicative m, Integral a) => a -> MList m b -> MList m b
takeML 0 _ = ML (pure Nil)
takeML n (ML xs) = ML $ bc Nil (\h t -> h :# takeML (n-1) t) <$> xs

dropML :: (Monad m, Integral a) => a -> MList m b -> MList m b
dropML 0 xs = xs
dropML n (ML xs) = ML $ xs >>= bc (return Nil) (\_ t -> runML $ dropML (n-1) t)

dropWhileML :: Monad m => (a -> Bool) -> MList m a -> MList m a
dropWhileML f (ML xs) = ML $ xs >>= bc (return Nil) (\h t -> if f h then runML (dropWhileML f t) else return (h :# t))

iterateML :: Functor m => (a -> m a) -> a -> MList m a
iterateML f x = ML $ (\y -> y :# iterateML f y) <$> f x

unfoldML :: Functor m => (b -> m (Maybe (a,b))) -> b -> MList m a
unfoldML f x = ML $ maybe Nil (\(a,b) -> a :# unfoldML f b) <$> f x

-- |Reverses an 'MList'. The list has to be finite.
reverseML :: Monad m => MList m a -> MList m a
reverseML xs = ML $ fromMonadic xs >>= (runML . toMonadic . reverse)

-- |Repeats an element a given number of times.
replicateML :: (Applicative m, Integral a) => a -> m b -> MList m b
replicateML n x = takeML n (repeatML x)

-- |Repeats an item infinitely often.
--
--  __NOTE__: You can directly replicate an 'MList' via @repeatML xs@, but the result won't be joinable. For that, you have to write
--            @repeatML (pure xs)@.
repeatML :: Functor m => m a -> MList m a
repeatML x = ML $ (:# repeatML x) <$> x

cycleML :: Monad m => MList m a -> MList m a
cycleML = join . repeatML . pure

elemML :: (Monad m, Eq a) => a -> MList m a -> m Bool
elemML x = nullML . dropWhileML (x ==)

notElemML :: (Monad m, Eq a) => a -> MList m a -> m Bool
notElemML x xs = not <$> elemML x xs

zipWithML :: Monad m
          => (a -> b -> m c)
          -> MList m a
          -> MList m b
          -> MList m c
zipWithML f (ML l1) (ML l2) = ML $ do
   l1' <- l1
   l2' <- l2
   case (l1', l2') of
      (x :# xs, y :# ys) -> (:# zipWithML f xs ys) <$> f x y
      _ -> return Nil

zipWith3ML :: Monad m
           => (a -> b -> c -> m d)
           -> MList m a
           -> MList m b
           -> MList m c
           -> MList m d
zipWith3ML f (ML l1) (ML l2) (ML l3) = ML $ do
   l1' <- l1
   l2' <- l2
   l3' <- l3
   case (l1', l2', l3') of
      (x :# xs, y :# ys, z :# zs) -> (:# zipWith3ML f xs ys zs) <$> f x y z
      _ -> return Nil

zipWith4ML :: Monad m
           => (a -> b -> c -> d -> m e)
           -> MList m a
           -> MList m b
           -> MList m c
           -> MList m d
           -> MList m e
zipWith4ML f (ML l1) (ML l2) (ML l3) (ML l4) = ML $ do
   l1' <- l1
   l2' <- l2
   l3' <- l3
   l4' <- l4
   case (l1', l2', l3', l4') of
      (x :# xs, y :# ys, z :# zs, u :# us) -> (:# zipWith4ML f xs ys zs us) <$> f x y z u
      _ -> return Nil

zipWith5ML :: Monad m
           => (a -> b -> c -> d -> e -> m f)
           -> MList m a
           -> MList m b
           -> MList m c
           -> MList m d
           -> MList m e
           -> MList m f
zipWith5ML f (ML l1) (ML l2) (ML l3) (ML l4) (ML l5) = ML $ do
   l1' <- l1
   l2' <- l2
   l3' <- l3
   l4' <- l4
   l5' <- l5
   case (l1', l2', l3', l4', l5') of
      (x :# xs, y :# ys, z :# zs, u :# us, v :# vs) -> (:# zipWith5ML f xs ys zs us vs) <$> f x y z u v
      _ -> return Nil

zipWith6ML :: Monad m
           => (a -> b -> c -> d -> e -> f -> m g)
           -> MList m a
           -> MList m b
           -> MList m c
           -> MList m d
           -> MList m e
           -> MList m f
           -> MList m g
zipWith6ML f (ML l1) (ML l2) (ML l3) (ML l4) (ML l5) (ML l6) = ML $ do
   l1' <- l1
   l2' <- l2
   l3' <- l3
   l4' <- l4
   l5' <- l5
   l6' <- l6
   case (l1', l2', l3', l4', l5', l6') of
      (x :# xs, y :# ys, z :# zs, u :# us, v :# vs, w :# ws) -> (:# zipWith6ML f xs ys zs us vs ws) <$> f x y z u v w
      _ -> return Nil

zipWith7ML :: Monad m
           => (a -> b -> c -> d -> e -> f -> g -> m h)
           -> MList m a
           -> MList m b
           -> MList m c
           -> MList m d
           -> MList m e
           -> MList m f
           -> MList m g
           -> MList m h
zipWith7ML f (ML l1) (ML l2) (ML l3) (ML l4) (ML l5) (ML l6) (ML l7) = ML $ do
   l1' <- l1
   l2' <- l2
   l3' <- l3
   l4' <- l4
   l5' <- l5
   l6' <- l6
   l7' <- l7
   case (l1', l2', l3', l4', l5', l6', l7') of
      (x :# xs, y :# ys, z :# zs, u :# us, v :# vs, w :# ws, a :# as) -> (:# zipWith7ML f xs ys zs us vs ws as) <$> f x y z u v w a
      _ -> return Nil

zipWith8ML :: Monad m
           => (a -> b -> c -> d -> e -> f -> g -> h -> m i)
           -> MList m a
           -> MList m b
           -> MList m c
           -> MList m d
           -> MList m e
           -> MList m f
           -> MList m g
           -> MList m h
           -> MList m i
zipWith8ML f (ML l1) (ML l2) (ML l3) (ML l4) (ML l5) (ML l6) (ML l7) (ML l8) = ML $ do
   l1' <- l1
   l2' <- l2
   l3' <- l3
   l4' <- l4
   l5' <- l5
   l6' <- l6
   l7' <- l7
   l8' <- l8
   case (l1', l2', l3', l4', l5', l6', l7', l8') of
      (x :# xs, y :# ys, z :# zs, u :# us, v :# vs, w :# ws, a :# as, b :# bs) -> (:# zipWith8ML f xs ys zs us vs ws as bs) <$> f x y z u v w a b
      _ -> return Nil


zipML :: Monad m
      => MList m a
      -> MList m b
      -> MList m (a,b)
zipML = zipWithML (\x y -> return (x,y))

zip3ML :: Monad m
       => MList m a
       -> MList m b
       -> MList m c
       -> MList m (a,b,c)
zip3ML = zipWith3ML (\x y z -> return (x,y,z))

zip4ML :: Monad m
       => MList m a
       -> MList m b
       -> MList m c
       -> MList m d
       -> MList m (a,b,c,d)
zip4ML = zipWith4ML (\x y z u -> return (x,y,z,u))

zip5ML :: Monad m
       => MList m a
       -> MList m b
       -> MList m c
       -> MList m d
       -> MList m e
       -> MList m (a,b,c,d,e)
zip5ML = zipWith5ML (\x y z u v -> return (x,y,z,u,v))

zip6ML :: Monad m
       => MList m a
       -> MList m b
       -> MList m c
       -> MList m d
       -> MList m e
       -> MList m f
       -> MList m (a,b,c,d,e,f)
zip6ML = zipWith6ML (\x y z u v w -> return (x,y,z,u,v,w))

zip7ML :: Monad m
       => MList m a
       -> MList m b
       -> MList m c
       -> MList m d
       -> MList m e
       -> MList m f
       -> MList m g
       -> MList m (a,b,c,d,e,f,g)
zip7ML = zipWith7ML (\x y z u v w a -> return (x,y,z,u,v,w,a))

zip8ML :: Monad m
       => MList m a
       -> MList m b
       -> MList m c
       -> MList m d
       -> MList m e
       -> MList m f
       -> MList m g
       -> MList m h
       -> MList m (a,b,c,d,e,f,g,h)
zip8ML = zipWith8ML (\x y z u v w a b -> return (x,y,z,u,v,w,a,b))
