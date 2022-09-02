{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Cost where

import qualified Data.Vector.Storable as V

import Types
import Blocks
import Codec.Picture (imageData)

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

lineCost :: ISLLine -> Int -> Integer
lineCost move blockSize = round (fromIntegral (baseCost move * size block0) / fromIntegral blockSize :: Double)

cost :: ISL -> Integer
cost = snd . foldr f ([block0], 0)
  where
    f move (blocks, c) =
      ( blockEffect move blocks
      , c + lineCost move (targetSize move blocks)
      )

targetSize :: ISLLine -> [Block] -> Int
targetSize isl blocks = case isl of
  LineCut b o l -> f b
  PointCut b p -> f b
  Color b c -> f b
  Swap b0 b1 -> f b0
  Merge b0 b1 -> max (f b0) (f b1)
  where
    f = size . flip lookupBlock blocks

similarity :: Img -> Img -> Integer
similarity a b = round $ 0.005 * go 0 componentDiffs
  where
    go total v | V.length v == 0 = total
    go total v = go (total + sqrt (V.sum (V.take 4 v))) (V.drop 4 v)
    componentDiffs = V.zipWith componentDifference (imageData a) (imageData b)
    componentDifference :: Word8 -> Word8 -> Double
    componentDifference c c' = (fromIntegral c - fromIntegral c') ** 2