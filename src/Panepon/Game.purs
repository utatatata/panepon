module Panepon.Game where

import Prelude
import Control.Comonad (extend)
import Data.List (List(..), (:))
import Data.List.DoubleZipper (DoubleZipper(..), left, right, upper, lower)
import Data.List.Zipper (Zipper(..))
import Data.Maybe (Maybe(..))

data Color = Red | Yellow | Green | LightBlue | Purple

derive instance eqColor :: Eq Color

data Panel
  = Panel Color
  -- | Ojama Size?

derive instance eqPanel :: Eq Panel

type Field = DoubleZipper (Maybe Panel)

neightbours :: Field -> Int
neightbours (DoubleZipper (Zipper
    _
    (Zipper _ Nothing _)
    _
  )) = 0
neightbours dz@(DoubleZipper (Zipper
    upperLines
    (Zipper leftPanels (Just panel) rightPanels)
    lowerLines
  )) =
    let
      uppers = case upperLines of
        Nil -> 0
        Cons (Zipper _ p _) _ ->
          if p == Just panel then
            case upper dz of
              Nothing -> 0
              Just dz' ->
                1 + (neightbours $ dz')
          else
            0
      lowers = case lowerLines of
        Nil -> 0
        Cons (Zipper _ p _) _ ->
          if p == Just panel then
            case lower dz of
              Nothing -> 0
              Just dz' ->
                1 + (neightbours $ dz')
          else
            0
      lefts = case leftPanels of
        Nil -> 0
        Cons p _ ->
          if p == Just panel then
            case left dz of
              Nothing -> 0
              Just dz' ->
                1 + (neightbours $ dz')
          else
            0
      rights = case rightPanels of
        Nil -> 0
        Cons p _ ->
          if p == Just panel then
            case right dz of
              Nothing -> 0
              Just dz' ->
                1 + (neightbours $ dz')
          else
            0
    in
      uppers + lowers + lefts + rights

eliminate :: Field -> Maybe Panel
eliminate f@(DoubleZipper (Zipper
    _
    (Zipper _ panel _)
    _
  )) = if neightbours f >= 3 then Nothing else panel

eliminateStep :: Field -> Field
eliminateStep = extend eliminate

drop :: Field -> Maybe Panel
drop (DoubleZipper (Zipper
    upperLines
    (Zipper _ Nothing _)
    _
  )) = case upperLines of
    Cons (Zipper _ (Just panel) _) _ ->
      Just panel
    _ ->
      Nothing
drop dz@(DoubleZipper (Zipper
    _
    (Zipper _ (Just panel) _)
    lowerLines
  )) = case lowerLines of
    Cons (Zipper _ Nothing _) _ ->
      Nothing
    _ ->
      Just panel

dropStep :: Field -> Field
dropStep = extend drop
