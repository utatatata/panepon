module Data.List.Zipper where

import Prelude

import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)

class LeftRight t where
  left :: forall a. t a -> Maybe (t a)
  right :: forall a. t a -> Maybe (t a)

class UpDown t where
  up :: forall a. t a -> Maybe (t a)
  down :: forall a. t a -> Maybe (t a)

data Zipper a = Zipper (List a) a (List a)

instance leftRightZipper :: LeftRight Zipper where
  left (Zipper Nil _ _) = Nothing
  left (Zipper (l:ls) c rs) = Just (Zipper ls l (c:rs))
  right (Zipper _ _ Nil) = Nothing
  right (Zipper ls c (r:rs)) = Just (Zipper (c:ls) r rs)

instance functorZipper :: Functor Zipper where
  map f (Zipper ls c rs) = Zipper (map f ls) (f c) (map f rs)

unfoldZipper :: forall a b. (b -> Maybe (Tuple a b)) -> (b -> a) -> (b -> Maybe (Tuple a b)) -> b -> Zipper a
unfoldZipper prev center next =
  Zipper <$> unfoldr prev <*> center <*> unfoldr next

iterateZipper :: forall a. (a -> Maybe a) -> (a -> Maybe a) -> a -> Zipper a
iterateZipper prev next = unfoldZipper (dup <=< prev) identity (dup <<< next)
  where
  dup a = Tuple a a

-- instance extendZipper :: Extend Zipper where
--   extend f z = 