{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Quads where

import Types hiding (bl, tr)
import Codec.Picture (imageWidth, imageHeight)
import ImageOps
import Cost
import Data.Foldable (minimumBy)
import Data.Function (on)
import qualified Data.Map as Map

fromImage :: Img -> Blocks -> ISL
fromImage img blocks
  = concat
  $ map (\(bid, Rect a b _) -> snd $ toISL img a b bid)
  $ Map.toList
  $ snd blocks

powOf2 :: Int -> Int
powOf2 w = 2 ^ (floor (log (fromIntegral w :: Double) / log 2) :: Int)

toISL :: Img -> Point -> Point -> BlockId -> (Integer, ISL)
toISL img (x0,y0) (x1,y1) blockId =
  let w = x1 - x0
      h = y1 - y0
      blockImg = region img (x0, y0) (x1, y1)
      fillCost = similarityWithAverage blockImg + imgLineCost img blockImg color_cost
      pointCutOptions = [
        let x = x0 + xi * w `div` 3
            y = y0 + yi * h `div` 3
            (blCost, blCode) = toISL img (x0, y0) (x, y) (blockId ++ [0])
            (brCost, brCode) = toISL img (x, y0) (x1, y) (blockId ++ [1])
            (trCost, trCode) = toISL img (x, y) (x1, y1) (blockId ++ [2])
            (tlCost, tlCode) = toISL img (x0, y) (x, y1) (blockId ++ [3])
            pointCutCost = imgLineCost img blockImg point_cut_cost + blCost + brCost + trCost + tlCost
        in (pointCutCost, PointCut blockId (x, y) : blCode ++ brCode ++ trCode ++ tlCode)
        | xi <- [1..2], yi <- [1..2]
        ]
      options =
        (fillCost, [Color blockId (averageColour blockImg)]) :
         pointCutOptions
  in if w < 50 || h < 50 then head options
    else minimumBy (compare `on` fst) options
