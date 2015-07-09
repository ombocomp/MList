{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

{- |Operations on monadic lists (MLists), which allow the lazy computation
    of stream of monadic values. The functions in this module largely mirror
    those found in 'Data.List'.
-}
module Data.List.Monadic (

  -- * Folds
  foldlML,
  foldrML,

  -- * Accumulating maps
  mapAccumlML,
  mapAccumrML,

  -- ** Unzipping
  unzipML,
  unzip3ML,
  unzip4ML,
  unzip5ML,
  unzip6ML,
  unzip7ML,
  ) where

import Control.Monad


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
