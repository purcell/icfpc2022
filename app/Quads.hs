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
import Data.List (sortOn)

fromImage :: Blocks -> Img -> ISL
fromImage blocks img
  = concat
  $ map (\bb -> snd $ toISL img (edges img) bb)
  $ Map.toList
  $ snd blocks

toISL :: Img -> Edges -> (BlockId, Block) -> (Integer, ISL)
toISL img edges (blockId, block@(Rect (x0,y0) (x1,y1) bim)) =
  let w = x1 - x0
      h = y1 - y0
      edgeXs = take 2 $ (filter (\x -> x > x0 + w `div` 5 && x < x1 - w `div` 5) $ fst edges) ++ [x0 + w `div` 3, x0 + 2 * w `div` 3]
      edgeYs = take 2 $ (filter (\y -> y > y0 + h `div` 5 && y < y1 - h `div` 5) $ snd edges) ++ [y0 + h `div` 3, y0 + 2 * h `div` 3]
      blockImg = region img (x0, y0) (x1, y1)
      fillCost = similarityWithAverage blockImg + imgLineCost img blockImg color_cost
      pointCutBaseCost = imgLineCost img blockImg point_cut_cost
      lineCutBaseCost = imgLineCost img blockImg line_cut_cost
      doNothingCost = similarity blockImg bim
      options =
        [ (fillCost, [Color blockId (averageColour blockImg)])
        , (doNothingCost, [])
        ] ++ [
        let pcBlocks = Blocks.pointCut (x, y) block
            (blCost, blCode) = toISL img edges (blockId ++ [0], pcBlocks !! 0)
            (brCost, brCode) = toISL img edges (blockId ++ [1], pcBlocks !! 1)
            (trCost, trCode) = toISL img edges (blockId ++ [2], pcBlocks !! 2)
            (tlCost, tlCode) = toISL img edges (blockId ++ [3], pcBlocks !! 3)
            cutCost = pointCutBaseCost + blCost + brCost + trCost + tlCost
        in (cutCost, PointCut blockId (x, y) : blCode ++ brCode ++ trCode ++ tlCode)
        | w > 40, h > 40, x <- edgeXs, y <- edgeYs
        ] ++ [
        let pcBlocks = Blocks.lineCut X x block
            (lCost, lCode) = toISL img edges (blockId ++ [0], pcBlocks !! 0)
            (rCost, rCode) = toISL img edges (blockId ++ [1], pcBlocks !! 1)
            cutCost = lineCutBaseCost + lCost + rCost
        in (cutCost, LineCut blockId X x : lCode ++ rCode)
        | w > 40, null edgeYs, x <- edgeXs
        ] ++ [
        let pcBlocks = Blocks.lineCut Y y block
            (bCost, bCode) = toISL img edges (blockId ++ [0], pcBlocks !! 0)
            (tCost, tCode) = toISL img edges (blockId ++ [1], pcBlocks !! 1)
            cutCost = pointCutBaseCost + bCost + tCost
        in (cutCost, LineCut blockId Y y : bCode ++ tCode)
        | h > 40, null edgeXs, y <- edgeYs
        ]
  in minimumBy (compare `on` fst) options

type Edges = ([Int], [Int])

edges :: Img -> Edges
edges img =
  ( bestEdges $ map (\x -> (x + 3, - diffSum img [((x, y), (x + 5, y)) | y <- [0..imageHeight img - 1]])) [0..imageWidth img - 6]
  , bestEdges $ map (\y -> (y + 3, - diffSum img [((x, y), (x, y + 5)) | x <- [0..imageWidth img - 1]])) [0..imageHeight img - 6]
  )

bestEdges :: [(Int, Double)] -> [Int]
bestEdges = map fst . sortOn snd . pickMiddle . groupCont . filter ((< -10000) . snd)

groupCont :: [(Int, Double)] -> [[(Int, Double)]]
groupCont [] = []
groupCont ((a,b):abs) = case groupCont abs of
  [] -> [[(a, b)]]
  abs':abbs -> if fst (head abs') - a == 1 then ((a, b):abs'):abbs else [(a, b)]:abs':abbs

pickMiddle :: [[(Int, Double)]] -> [(Int, Double)]
pickMiddle = map (\l -> l !! (length l `div` 2))

diffSum :: Img -> [(Point, Point)] -> Double
diffSum img = sum . map (\(p0, p1) -> colorDiff (pixelAt img p0) (pixelAt img p1))