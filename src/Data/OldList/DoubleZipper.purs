module Data.OldList.DoubleZipper where

import Prelude
import Control.Comonad (class Comonad, extract, duplicate)
import Control.Extend (class Extend, extend)
import Data.List (List(..), (:), drop)
import Data.OldList.Zipper (Zipper(..))
import Data.OldList.Zipper as Z
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)

newtype DoubleZipper a
  = DoubleZipper (Zipper (Zipper a))

instance functorDoubleZipper :: Functor DoubleZipper where
  map f (DoubleZipper zz) = DoubleZipper (map (map f) zz)

upper :: forall a. DoubleZipper a -> Maybe (DoubleZipper a)
upper (DoubleZipper zz) = map DoubleZipper $ Z.left zz

lower :: forall a. DoubleZipper a -> Maybe (DoubleZipper a)
lower (DoubleZipper zz) = map DoubleZipper $ Z.right zz

left :: forall a. DoubleZipper a -> Maybe (DoubleZipper a)
left (DoubleZipper zz) = map DoubleZipper $ traverse Z.left zz

right :: forall a. DoubleZipper a -> Maybe (DoubleZipper a)
right (DoubleZipper zz) = map DoubleZipper $ traverse Z.right zz

iterateMapLeft :: forall a. Zipper (Zipper a) -> List (Zipper (Zipper a))
iterateMapLeft zz =
  zz
    : case traverse Z.left zz of
        Nothing -> Nil
        Just zz' -> iterateMapLeft zz'

iterateMapRight :: forall a. Zipper (Zipper a) -> List (Zipper (Zipper a))
iterateMapRight zz =
  zz
    : case traverse Z.right zz of
        Nothing -> Nil
        Just zz' -> iterateMapRight zz'

toList :: forall a. DoubleZipper a -> List (List a)
toList (DoubleZipper zz) = Z.toList $ map Z.toList $ zz

instance extendDoubleZipper :: Extend DoubleZipper where
  extend f (DoubleZipper zz) = map f $ map DoubleZipper <<< DoubleZipper <<< roll $ roll zz
    where
    roll :: forall a. Zipper (Zipper a) -> Zipper (Zipper (Zipper a))
    roll zz = Zipper (drop 1 $ iterateMapLeft zz) zz (drop 1 $ iterateMapRight zz)

instance comonadDoubleZipper :: Comonad DoubleZipper where
  extract (DoubleZipper zz) = extract $ extract zz
