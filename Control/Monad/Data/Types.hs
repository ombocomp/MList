{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.Monad.Data.Types (
   BiCons(..),
   MList(..),
   MTree(..),
   bc,
   ) where

import Control.Monad.Data.Class
import Control.Monad.Data.Foldable
import Data.Tree

-- |A type that's either Nil or a pair of elements.
data BiCons a b = Nil | a :# b

-- |Case distinction for 'BiCons'.
bc :: c -> (a -> b -> c) -> BiCons a b -> c
{-# INLINE bc #-}
bc nil cons c = case c of Nil      -> nil
                          (h :# t) -> cons h t

-- |Monadic list that can be evaluated incrementally.
newtype MList m a = ML{runML :: m (BiCons a (MList m a))}
-- |Monadic tree that can be evaluated incrementally.
newtype MTree m a = MT{runMT :: m (a,[MTree m a])}

conc :: Monad m => MList m (MList m a) -> MList m a
conc (ML xs) = ML $ do
   xs >>= \case {Nil -> return Nil; (h :# t) -> runML $ h `mappend` conc t}

concMap :: Monad m => (a -> MList m b) -> MList m a -> MList m b
concMap f = conc . fmap f

instance MonadicStructure MList where
   type Target MList = []

   toMonadic [] = ML (pure Nil)
   toMonadic (x:xs) = ML (pure (x :# toMonadic xs))

   fromMonadic (ML xs) = xs >>= \case{Nil -> return []; (h :# t) -> (h:) <$> fromMonadic t}

instance Functor m => Functor (MList m) where
   fmap f (ML list) = ML (fmap go list)
      where
         go Nil = Nil
         go (x :# xs) = f x :# fmap f xs

instance Monad m => Monoid (MList m a) where
   mempty = ML (pure Nil)
   mappend (ML xs) (ML ys) = ML $
      xs >>= \case {Nil -> ys; (h :# t) -> return $ h :# mappend t (ML ys)}

instance Monad m => Applicative (MList m) where
   pure x = ML $ pure $ x :# ML (pure Nil)
   fs <*> xs = concMap (<$> xs) fs

instance Monad m => Monad (MList m) where
   x >>= f = conc (fmap f x)

instance FoldableM MList where
   type Con MList r = Monad r
   foldrM f acc (ML xs) = xs >>= \case{Nil -> return acc; (h :# t) -> foldrM f acc t >>= f h}

instance MonadicStructure MTree where
   type Target MTree = Tree

   toMonadic (Node n []) = MT $ pure $ (n, [])
   toMonadic (Node n ns) = MT $ pure $ (n, map toMonadic ns)

   fromMonadic (MT xs) = xs >>= \case (n,[]) -> return (Node n [])
                                      (n,ns) -> Node n <$> mapM fromMonadic ns

instance Functor m => Functor (MTree m) where
   fmap f (MT ns) = MT (fmap go ns)
      where
         go (n,[]) = (f n, [])
         go (n,ns) = (f n, fmap (fmap f) ns)

{-
instance Monad m => Applicative (MTree m) where
   pure x = MT $ pure $ (x,[]) 
   fs <*> 

    Node f tfs <*> tx@(Node x txs) =
        Node (f x) (map (f <$>) txs ++ map (<*> tx) tfs)

   Node f [] <*> tx@(Node x txs) =
        Node (f x) $    map (fmap f) txs  ++ map (<*> tx) tfs


   Node (+10) [] <*> Node 2 [Node 1 [], Node 3]

   Node 12 $ [fmap (+10) 1, fmap (+10) 3] 

-}

--instance FoldableM MTree where
--   type Con MTree r = Monad r
--   foldrM f acc (MT xs) = 
