{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.Monad.Data.Foldable where

import GHC.Exts (Constraint)

newtype EndoM m a = EndoM{appEndoM :: a -> m a}

-- |Foldable monadic structures.
class FoldableM m where
   -- |The constraint imposed on the return type. 'Monad' by default.
   type Con m (r :: * -> *) :: Constraint
   type Con m r = Monad r
   
   -- |Combines the elements of a structure using a monoid.
   foldM :: (Applicative r, Monoid a, Con m r) => m r a -> r a
   foldM = foldMapM pure

   -- |Maps the elements to a monoid and combines them.
   foldMapM :: (Monoid b, Functor r, Con m r) => (a -> r b) -> m r a -> r b
   foldMapM f = foldrM (\x acc -> (`mappend` acc) <$> f x) mempty

   -- |Monadic right-fold.
   foldrM :: Con m r => (a -> b -> r b) -> b -> m r a -> r b

-- |Show for monadic structures.
class ShowM m where
   -- |The constraint on the return type constructor. 'Monad' by default.
   type ShowCon m (r :: * -> *) :: Constraint
   type ShowCon m r = Monad r

   showM :: ShowCon m r => m r a -> r String

-- |Eq for monadic structures.
class EqM m where
   -- |The constraint on the return type constructor. 'Monad'
   --  by default, but can be any subclass of 'Functor'.
   type EqCon m (r :: * -> *) :: Constraint
   type EqCon m r = Monad r

   (==#) :: (Functor r, EqCon m r, Eq a) => m r a -> m r a -> r Bool
   x ==# y = not <$> (x /=# y)
   (/=#) :: (Functor r, EqCon m r, Eq a) => m r a -> m r a -> r Bool
   x /=# y = not <$> (x ==# y)

   {-# MINIMAL (==#), (/=#) #-}

-- |Ord for monadic structures.
class EqM m => OrdM m where
   -- |The constraint on the return type constructor. 'Monad'
   --  by default, but can be any subclass of 'Functor'.
   type OrdCon m (r :: * -> *) :: Constraint
   type OrdCon m r = Monad r

   compareM :: (OrdCon m r, Ord a) => m r a -> m r a -> r Ordering

   (<=#) :: (Functor r, OrdCon m r, Ord a) => m r a -> m r a -> r Bool
   x <=# y = (GT /=) <$> compareM x y

   (>=#) :: (Functor r, OrdCon m r, Ord a) => m r a -> m r a -> r Bool
   x >=# y = (LT /=) <$> compareM x y

   (<#) :: (Functor r, OrdCon m r, Ord a) => m r a -> m r a -> r Bool
   x <# y = (LT ==) <$> compareM x y

   (>#) :: (Functor r, OrdCon m r, Ord a) => m r a -> m r a -> r Bool
   x ># y = (GT ==) <$> compareM x y

   maxM :: (Functor r, OrdCon m r, Ord a) => m r a -> m r a -> r (m r a)
   maxM x y = \case{GT -> x; _ -> y} <$> compareM x y

   minM :: (Functor r, OrdCon m r, Ord a) => m r a -> m r a -> r (m r a)
   minM = flip maxM

   {-# MINIMAL compareM, (<=#) #-}
