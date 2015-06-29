{-# LANGUAGE LambdaCase #-}

module Control.Monad.Data.List.Internal where

import Control.Monad.Data.Types

maybeHeadML :: Functor m => (a -> b) -> b -> MList m a -> m b
maybeHeadML good bad (ML xs) = fmap (\case{Nil -> bad; (h :# _) -> good h}) xs

-- |Returns the tail of an MList.
maybeTailML :: Functor m => (MList m a -> b) -> b -> MList m a -> m b
maybeTailML good bad (ML xs) = fmap (\case{Nil -> bad; (_ :# t) -> good t}) xs


-- |Returns an MList without its last element.
maybeInitML :: Applicative m => (MList m a -> b) -> b -> MList m a -> m b
maybeInitML good bad (ML xs) = fmap (\case{Nil -> bad; xs' -> good $ init' xs'}) xs
   where
      init' (h :# (ML t)) = ML $ fmap (\case Nil -> (h :# ML (pure Nil))
                                             xs' -> (h :# init' xs')) t
      init' _ = error "BUG: failed pattern match in maybeInitML"

-- |Returns the last element of an MList.
maybeLastML :: Monad m => (a -> b) -> b -> MList m a -> m b
maybeLastML good bad (ML xs) = xs >>= (\case{Nil -> return bad; xs' -> good <$> last' xs'})
   where
      last' (h :# (ML t)) = t >>= \case Nil -> return h
                                        xs' -> last' xs'
      last' _ = error "BUG: failed pattern match in maybeLastML"
