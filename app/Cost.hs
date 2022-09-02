{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Cost where

import Lens.Micro (Traversal', ix, over)

import Types
import Blocks

size :: Block -> Int
size (SimpleBlock (Rect (x0, y0) (x1, y1))) = (x1 - x0) * (y1 - y0)
size _ = error "size called on destroyed block"

baseCost :: ISLLine -> Int
baseCost = \case
  LineCut{} -> 7
  PointCut{} -> 10
  Color{} -> 5
  Swap{} -> 3
  Merge{} -> 1

lineCost :: ISLLine -> Block -> Integer
lineCost move block = round (fromIntegral (baseCost move * size block0) / fromIntegral (size block) :: Double)

cost :: ISL -> Integer
cost = snd . foldr f ([block0], 0)
  where
    f move (blocks, c) =
      ( blockEffect move blocks
      , c + lineCost move (targetBlock move blocks)
      )

targetBlock :: ISLLine -> [Block] -> Block
targetBlock = lookupBlock . \case
  LineCut b o l -> b
  PointCut b p -> b
  Color b c -> b
  Swap b0 b1 -> b0
  Merge b0 b1 -> b0 -- ? TODO
