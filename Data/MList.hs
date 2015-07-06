{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

{- |Operations on monadic lists (MLists), which allow the lazy computation
    of stream of monadic values. The functions in this module largely mirror
    those found in 'Data.List'.
-}
module Data.List.Monadic (
  MList(..),

  -- * Conversion to and from lists
  fromMList,
  toMList,

  -- * Show class for MLists
  MShow(..),

  -- * Basic operations
  (+++),
  headML,
  lastML,
  tailML,
  initML,
  nullML,
  lengthML,

  -- * Transformations
  mapML,
  reverseML,

  -- * Folds
  foldlML,
  foldrML,
  unfoldML,

  -- * Accumulating maps
  mapAccumlML,
  mapAccumrML,

  -- * Infinite MLists
  iterateML,
  repeatML,
  replicateML,
  cycleML,

  -- * Sublists
  takeML,
  dropML,
  takeWhileML,
  dropWhileML,

  -- * Searching MLists
  elemML,
  notElemML,

  -- * Zipping and unzipping MLists

  -- ** Zipping
  zipML,
  zip3ML,
  zip4ML,
  zip5ML,
  zip6ML,
  zip7ML,

  -- ** Zipping with functions
  zipWithML,
  zipWith3ML,
  zipWith4ML,
  zipWith5ML,
  zipWith6ML,
  zipWith7ML,

  -- ** Unzipping
  unzipML,
  unzip3ML,
  unzip4ML,
  unzip5ML,
  unzip6ML,
  unzip7ML,
  ) where

import Control.Monad

-- |A list of type @a@ whose tail has type @m [a]@ instead of @[a]@.
--  This allows for the lazy generation of elements, even if the list
--  is a stream of monadic values.
data MList m a = MNil | a :# (m (MList m a))
infixr 5 :#

-- |Concatenates two MLists.
(++#) :: Monad m => MList m a -> MList m a -> MList m a
MNil ++# ys = ys
(x :# xs) ++# ys = x :# fmap (++# ys) xs



-- |Applies a function to every element of an MList.
mapML :: Monad m => (a -> m b) -> MList m a -> m (MList m b)
mapML _ MNil = return MNil
mapML f (x :# xs) = do y <- f x
                       return $ y :# (xs >>= mapML f)

-- |Combines 'foldlML' and 'mapML', both applying a function to
--  every element and accumulating a value.
mapAccumlML :: Monad m
            => (acc -> a -> m (acc, b))
            -> acc
            -> MList m a
            -> m (acc, MList m b)
mapAccumlML _ acc MNil = return (acc, MNil)
mapAccumlML f acc (x :# xs) = do (acc', y) <- f acc x
                                 (acc'', ys) <- xs >>= mapAccumlML f acc'
                                 return (acc'', y :# return ys)

-- |Combines 'foldrML' and 'mapML', both applying a function to
--  every element and accumulating a value.
mapAccumrML :: Monad m
            => (acc -> a -> m (acc, b))
            -> acc
            -> MList m a
            -> m (acc, MList m b)
mapAccumrML _ acc MNil = return (acc, MNil)
mapAccumrML f acc (x :# xs) = do (acc', ys) <- xs >>= mapAccumrML f acc
                                 (acc'', y) <- f acc' x
                                 return (acc'', y :# return ys)

-- |Constructs the infinite MList @[x, f x, f (f x), ...]@.
iterateML :: Monad m => (a -> m a) -> a -> MList m a
iterateML f x = x :# liftM (iterateML f) (f x)

-- |Repeats the same element infinitely.
repeatML :: Monad m => m a -> m (MList m a)
repeatML x = do x' <- x
                return $ x' :# repeatML x

-- |Creates an MList consisting of n copies of an element.
replicateML :: Monad m => Int -> m a -> m (MList m a)
replicateML n x | n <= 0 = return MNil
                | n > 0  = do x' <- x
                              return (x' :# replicateML (n-1) x) 

-- |Repeats an MList infinitely.
cycleML :: Monad m => MList m a -> MList m a
cycleML MNil = error "cycleML: tail of empty MList!"
cycleML xs = xs +++ cycleML xs

-- |Returns True if an MList contains a given element.
--  If the list is infinite, this is only a semi-decision procedure.
elemML :: (Monad m, Eq a) => a -> MList m a -> m Bool
elemML _ MNil = return False
elemML x (y :# ys) | x == y    = return True
                   | otherwise = ys >>= elemML y

-- |The negation of 'elemML'.
notElemML :: (Monad m, Eq a) => a -> MList m a -> m Bool
notElemML x ys = liftM not (elemML x ys)

-- |Zips two MLists.
zipML :: Monad m
      => MList m a
      -> MList m b
      -> MList m (a,b)
zipML = zipWithML (,)

-- |Zips three MLists.
zip3ML :: Monad m
       => MList m a
       -> MList m b
       -> MList m c
       -> MList m (a,b,c)
zip3ML = zipWith3ML (,,)

-- |Zips four MLists.
zip4ML :: Monad m
       => MList m a
       -> MList m b
       -> MList m c
       -> MList m d
       -> MList m (a,b,c,d)
zip4ML = zipWith4ML (,,,)

-- |Zips five MLists.
zip5ML :: Monad m
       => MList m a
       -> MList m b
       -> MList m c
       -> MList m d
       -> MList m e
       -> MList m (a,b,c,d,e)
zip5ML = zipWith5ML (,,,,)

-- |Zips six MLists.
zip6ML :: Monad m
       => MList m a
       -> MList m b
       -> MList m c
       -> MList m d
       -> MList m e
       -> MList m f
       -> MList m (a,b,c,d,e,f)
zip6ML = zipWith6ML (,,,,,)

-- |Zips seven MLists.
zip7ML :: Monad m
       => MList m a
       -> MList m b
       -> MList m c
       -> MList m d
       -> MList m e
       -> MList m f
       -> MList m g
       -> MList m (a,b,c,d,e,f,g)
zip7ML = zipWith7ML (,,,,,,)

-- |Zips two lists with a function.
zipWithML :: Monad m
        => (a -> b -> c)
        -> MList m a
        -> MList m b
        -> MList m c
zipWithML f (x :# xs) (y :# ys) =
  (f x y) :# do xs' <- xs
                ys' <- ys
                return (zipWithML f xs' ys')
zipWithML _ _ _ = MNil

-- |Zips three lists with a function.
zipWith3ML :: Monad m
        => (a -> b -> c -> d)
        -> MList m a
        -> MList m b
        -> MList m c
        -> MList m d
zipWith3ML f (x :# xs) (y :# ys) (z :# zs) =
  (f x y z) :# do xs' <- xs
                  ys' <- ys
                  zs' <- zs
                  return (zipWith3ML f xs' ys' zs')
zipWith3ML _ _ _ _ = MNil

-- |Zips four lists with a function.
zipWith4ML :: Monad m
        => (a -> b -> c -> d -> e)
        -> MList m a
        -> MList m b
        -> MList m c
        -> MList m d
        -> MList m e
zipWith4ML f (x :# xs) (y :# ys) (z :# zs) (u :# us) =
  (f x y z u) :# do xs' <- xs
                    ys' <- ys
                    zs' <- zs
                    us' <- us
                    return (zipWith4ML f xs' ys' zs' us')
zipWith4ML _ _ _ _ _ = MNil

-- |Zips five lists with a function.
zipWith5ML :: Monad m
        => (a -> b -> c -> d -> e -> f)
        -> MList m a
        -> MList m b
        -> MList m c
        -> MList m d
        -> MList m e
        -> MList m f
zipWith5ML f (x :# xs) (y :# ys) (z :# zs) (u :# us) (v :# vs) =
  (f x y z u v) :# do xs' <- xs
                      ys' <- ys
                      zs' <- zs
                      us' <- us
                      vs' <- vs
                      return (zipWith5ML f xs' ys' zs' us' vs')
zipWith5ML _ _ _ _ _ _ = MNil

-- |Zips six lists with a function.
zipWith6ML :: Monad m
        => (a -> b -> c -> d -> e -> f -> g)
        -> MList m a
        -> MList m b
        -> MList m c
        -> MList m d
        -> MList m e
        -> MList m f
        -> MList m g
zipWith6ML f (x :# xs) (y :# ys) (z :# zs) (u :# us) (v :# vs) (w :# ws) =
  (f x y z u v w) :# do xs' <- xs
                        ys' <- ys
                        zs' <- zs
                        us' <- us
                        vs' <- vs
                        ws' <- ws
                        return (zipWith6ML f xs' ys' zs' us' vs' ws')
zipWith6ML _ _ _ _ _ _ _ = MNil

-- |Zips seven lists with a function.
zipWith7ML :: Monad m
        => (a -> b -> c -> d -> e -> f -> g -> h)
        -> MList m a
        -> MList m b
        -> MList m c
        -> MList m d
        -> MList m e
        -> MList m f
        -> MList m g
        -> MList m h
zipWith7ML f (x :# xs) (y :# ys) (z :# zs) (u :# us) (v :# vs) (w :# ws) (a :# as) =
  (f x y z u v w a) :# do xs' <- xs
                          ys' <- ys
                          zs' <- zs
                          us' <- us
                          vs' <- vs
                          ws' <- ws
                          as' <- as
                          return (zipWith7ML f xs' ys' zs' us' vs' ws' as')
zipWith7ML _ _ _ _ _ _ _ _ = MNil

-- |Transforms a list of pairs into a list of components.
unzipML :: Monad m
        => MList m (a,b)
        -> m (MList m a, MList m b)
unzipML = foldrML (\(a,b) (as,bs) -> return (a :# return as, b :# return bs))
                  (MNil, MNil)

-- |Transforms a list of pairs into a list of components.
unzip3ML :: Monad m
        => MList m (a,b,c)
        -> m (MList m a, MList m b, MList m c)
unzip3ML = foldrML (\(a,b,c) (as,bs,cs) -> return (a :# return as, b :# return bs, c :# return cs))
                   (MNil, MNil, MNil)

-- |Transforms a list of pairs into a list of components.
unzip4ML :: Monad m
        => MList m (a,b,c,d)
        -> m (MList m a, MList m b, MList m c, MList m d)
unzip4ML = foldrML (\(a,b,c,d) (as,bs,cs,ds) -> return (a :# return as, b :# return bs, c :# return cs,
                                                        d :# return ds))
                   (MNil, MNil, MNil, MNil)

-- |Transforms a list of pairs into a list of components.
unzip5ML :: Monad m
        => MList m (a,b,c,d,e)
        -> m (MList m a, MList m b, MList m c, MList m d, MList m e)
unzip5ML = foldrML (\(a,b,c,d,e) (as,bs,cs,ds,es) -> return (a :# return as, b :# return bs, c :# return cs,
                                                             d :# return ds, e :# return es))
                   (MNil, MNil, MNil, MNil, MNil)

-- |Transforms a list of pairs into a list of components.
unzip6ML :: Monad m
        => MList m (a,b,c,d,e,f)
        -> m (MList m a, MList m b, MList m c, MList m d, MList m e, MList m f)
unzip6ML = foldrML (\(a,b,c,d,e,f) (as,bs,cs,ds,es,fs) -> return (a :# return as, b :# return bs, c :# return cs,
                                                                  d :# return ds, e :# return es, f :# return fs))
                   (MNil, MNil, MNil, MNil, MNil, MNil)

-- |Transforms a list of pairs into a list of components.
unzip7ML :: Monad m
        => MList m (a,b,c,d,e,f,g)
        -> m (MList m a, MList m b, MList m c, MList m d, MList m e, MList m f, MList m g)
unzip7ML = foldrML (\(a,b,c,d,e,f,g) (as,bs,cs,ds,es,fs,gs) -> return (a :# return as, b :# return bs, c :# return cs,
                                                                       d :# return ds, e :# return es, f :# return fs,
                                                                       g :# return gs))
                   (MNil, MNil, MNil, MNil, MNil, MNil, MNil)
