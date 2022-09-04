{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Quads where

import Types hiding (bl, tr)
import Codec.Picture (imageWidth, imageHeight)
import ImageOps
import Cost
import Data.Foldable (minimumBy)
import Data.Function (on)
import qualified Data.Map.Strict as Map
import qualified Blocks
import Debug.Trace

fromImage :: Blocks -> Img -> ISL
fromImage blocks img
  = concat
  $ map (\bb -> snd $ toISL img bb)
  $ Map.toList
  $ snd blocks

powOf2 :: Int -> Int
powOf2 w = 2 ^ (floor (log (fromIntegral w :: Double) / log 2) :: Int)

toISL :: Img -> (BlockId, Block) -> (Integer, ISL)
toISL img (blockId, block@(Rect (x0,y0) (x1,y1) bim)) =
  let w = x1 - x0
      h = y1 - y0
      blockImg = region img (x0, y0) (x1, y1)
      fillCost = similarityWithAverage blockImg + imgLineCost img blockImg color_cost
      doNothingCost = similarity blockImg bim
      pointCutOptions = [
        let x = x0 + xi * w `div` 3
            y = y0 + yi * h `div` 3
            pcBlocks = Blocks.pointCut (x, y) block
            (blCost, blCode) = toISL img (blockId ++ [0], pcBlocks !! 0)
            (brCost, brCode) = toISL img (blockId ++ [1], pcBlocks !! 1)
            (trCost, trCode) = toISL img (blockId ++ [2], pcBlocks !! 2)
            (tlCost, tlCode) = toISL img (blockId ++ [3], pcBlocks !! 3)
            pointCutCost = imgLineCost img blockImg point_cut_cost + blCost + brCost + trCost + tlCost
        in (pointCutCost, PointCut blockId (x, y) : blCode ++ brCode ++ trCode ++ tlCode)
        | xi <- [1..2], yi <- [1..2]
        ]
      options =
        [ (fillCost, [Color blockId (averageColour blockImg)])
        , (doNothingCost, [])
        ] ++ if w < 50 || h < 50 then [] else pointCutOptions
  in minimumBy (compare `on` fst) options
