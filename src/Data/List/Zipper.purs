module Data.List.Zipper where

import Prelude

import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.Foldable (class Foldable, foldMap)
import Data.List (List(..), (:), drop)
import Data.Maybe (Maybe(..))
import Data.Monoid.Endo (Endo(..))
import Data.Monoid.Dual (Dual(..))
import Data.Newtype (unwrap)
import Data.Traversable (class Traversable, sequence, traverse)

data Zipper a = Zipper (List a) a (List a)

left :: forall a. Zipper a -> Maybe (Zipper a)
left (Zipper (l:ls) c rs) = Just $ Zipper ls l (c:rs)
left _ = Nothing

right :: forall a. Zipper a -> Maybe (Zipper a)
right (Zipper ls c (r:rs)) = Just $ Zipper (c:ls) r rs
right _ = Nothing

first :: forall a. Zipper a -> Zipper a
first z@(Zipper Nil _ _) = z
first (Zipper (l:ls) c rs) = first $ Zipper ls l (c:rs)

last :: forall a. Zipper a -> Zipper a
last z@(Zipper _ _ Nil) = z
last (Zipper ls c (r:rs)) = last $ Zipper (c:ls) r rs

iterateLeft :: forall a. Zipper a -> List (Zipper a)
iterateLeft z = z : case left z of
  Nothing -> Nil
  Just z' -> iterateLeft z'

iterateRight :: forall a. Zipper a -> List (Zipper a)
iterateRight z = z : case right z of
  Nothing -> Nil
  Just z' -> iterateRight z'

instance functorZipper :: Functor Zipper where
  map f (Zipper ls c rs) = Zipper (map f ls) (f c) (map f rs)

instance foldableZipper :: Foldable Zipper where
  foldr f a z = unwrap (foldMap (Endo <<< f) z) a
  foldl f a z = unwrap (unwrap (foldMap (Dual <<< Endo <<< flip f) z)) a
  foldMap f (Zipper ls c rs) = foldMap f ls <> f c <> foldMap f rs

instance traversableZipper :: Traversable Zipper where
  traverse f (Zipper ls c rs) = Zipper <$> traverse f ls <*> f c <*> traverse f rs
  sequence = traverse identity

instance extendZipper :: Extend Zipper where
  extend f z = map f $ Zipper (drop 1 $ iterateLeft z) z (drop 1 $ iterateRight z)

instance comonadZipper :: Comonad Zipper where
  extract (Zipper _ c _) = c
