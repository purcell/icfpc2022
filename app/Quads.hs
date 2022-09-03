{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Quads where

import Types hiding (bl, tr)
import Codec.Picture (mixWith, imageWidth, imageHeight, generateImage)
import ImageOps
import Cost
import Cost (point_cut_cost)

fromImage :: Img -> ISL
fromImage img = snd $ toISL img (0, 0) (imageWidth img, imageHeight img) [0]

powOf2 :: Int -> Int
powOf2 w = 2 ^ (floor (log (fromIntegral w :: Double) / log 2) :: Int)

toISL :: Img -> Point -> Point -> BlockId -> (Integer, ISL)
toISL img (x0,y0) (x1,y1) blockId =
  let w = x1 - x0
      h = y1 - y0
      x = x0 + w `div` 2
      y = y0 + h `div` 2
      bl = region img (x0, y0) (x, y)
      br = region img (x, y0) (x1, y)
      tr = region img (x, y) (x1, y1)
      tl = region img (x0, y) (x, y1)
      all = region img (x0, y0) (x1, y1)
      (blCost, blCode) = toISL img (x0, y0) (x, y) (blockId ++ [0])
      (brCost, brCode) = toISL img (x, y0) (x1, y) (blockId ++ [1])
      (trCost, trCode) = toISL img (x, y) (x1, y1) (blockId ++ [2])
      (tlCost, tlCode) = toISL img (x0, y) (x, y1) (blockId ++ [3])
      fillCost = similarityWithAverage all + imgLineCost img all color_cost
      pointCutCost = imgLineCost img all point_cut_cost + blCost + brCost + trCost + tlCost
  in if length blockId > 4 || fillCost < pointCutCost
    then
      (fillCost, [Color blockId (averageColour (region img (x0,y0) (x1,y1)))])
    else
      (pointCutCost, [PointCut blockId (x, y)] ++ blCode ++ brCode ++ trCode ++ tlCode)
