module Panepon.Game where

import Prelude
import Control.Comonad (extend)
import Data.List (List(..), (:))
import Data.OldList.DoubleZipper (DoubleZipper(..), left, right, upper, lower)
import Data.OldList.Zipper (Zipper(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (overF)
import Data.Traversable (sequence)
import Data.Unfoldable (replicate)
import Effect (Effect)
import Effect.Random (randomInt)

data Panel
  = Heart
  | Star
  | Circle
  | Diamond
  | Triangle

-- | Ojama Size?
derive instance eqPanel :: Eq Panel

type Field
  = DoubleZipper (Maybe Panel)

rows :: Int
rows = 12

cols :: Int
cols = 6

initHeight :: Int
initHeight = 6

init :: Effect Field
init =
  let
    l1 = emptyLine

    l2_6 = replicate 6 emptyLine
  in
    do
      l7 <- line7
      l8 <- line8
      l9_12 <- sequence $ replicate 4 fullLine
      pure $ DoubleZipper $ Zipper Nil l1 $ l2_6 <> l7 : l8 : l9_12
  where
  panel = do
    n <- randomInt 1 5
    pure $ Just
      $ case n of
          1 -> Heart
          2 -> Star
          3 -> Circle
          4 -> Triangle
          _ -> Diamond

  emptyLine = Zipper Nil Nothing $ replicate (cols - 1) Nothing

  fullLine = do
    col1 <- panel
    col2_6 <- sequence (replicate (cols - 1) panel)
    pure $ Zipper Nil col1 col2_6

  line7 = do
    col4 <- panel
    pure $ Zipper Nil Nothing $ Nothing : Nothing : col4 : Nothing : Nothing : Nil

  line8 = do
    col2_6 <- sequence (replicate (cols - 1) panel)
    pure $ Zipper Nil Nothing col2_6

neighbours :: Field -> Maybe { panel :: Panel, n :: Int }
neighbours ( DoubleZipper
    ( Zipper
      upperLines
      (Zipper leftPanels it rightPanels)
      lowerLines
  )
) = case it of
  Nothing -> Nothing
  Just p ->
    Just
      { panel: p
      , n:
        ( case upperLines of
            Cons (Zipper _ (Just p') _) _ -> if p == p' then 1 else 0
            _ -> 0
        )
          + ( case lowerLines of
                Cons (Zipper _ (Just p') _) _ -> if p == p' then 1 else 0
                _ -> 0
            )
          + ( case leftPanels of
                Cons (Just p') _ -> if p == p' then 1 else 0
                _ -> 0
            )
          + ( case rightPanels of
                Cons (Just p') _ -> if p == p' then 1 else 0
                _ -> 0
            )
      }

neighbours2 :: DoubleZipper (Maybe { panel :: Panel, n :: Int }) -> Int
neighbours2 ( DoubleZipper
    ( Zipper
      _
      (Zipper _ Nothing _)
      _
  )
) = 0

neighbours2 ( DoubleZipper
    ( Zipper
      upperLines
      (Zipper leftPanels (Just { panel, n }) rightPanels)
      lowerLines
  )
) =
  n
    + ( case upperLines of
          Cons (Zipper _ (Just { panel: p, n: m }) _) _ -> if panel == p then m else 0
          _ -> 0
      )
    + ( case lowerLines of
          Cons (Zipper _ (Just { panel: p, n: m }) _) _ -> if panel == p then m else 0
          _ -> 0
      )
    + ( case leftPanels of
          Cons (Just { panel: p, n: m }) _ -> if panel == p then m else 0
          _ -> 0
      )
    + ( case rightPanels of
          Cons (Just { panel: p, n: m }) _ -> if panel == p then m else 0
          _ -> 0
      )

eliminate :: Field -> Maybe Panel
eliminate f@( DoubleZipper
    ( Zipper
      _
      (Zipper _ panel _)
      _
  )
) = if (neighbours2 $ extend neighbours f) >= 3 then Nothing else panel

eliminateStep :: Field -> Field
eliminateStep = extend eliminate

drop :: Field -> Maybe Panel
drop ( DoubleZipper
    ( Zipper
      upperLines
      (Zipper _ Nothing _)
      _
  )
) = case upperLines of
  Cons (Zipper _ (Just panel) _) _ -> Just panel
  _ -> Nothing

drop dz@( DoubleZipper
    ( Zipper
      _
      (Zipper _ (Just panel) _)
      lowerLines
  )
) = case lowerLines of
  Cons (Zipper _ Nothing _) _ -> Nothing
  _ -> Just panel

dropStep :: Field -> Field
dropStep = extend drop
