{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Cost where

import qualified Data.Vector.Storable as V

import Types
import Blocks
import Codec.Picture (imageData, imageWidth, imageHeight)
import Data.Foldable (foldl')
import ImageOps (averageColour, filledWith, imgSize)

point_cut_cost :: Int
point_cut_cost = 10

line_cut_cost :: Int
line_cut_cost = 7

color_cost :: Int
color_cost = 5

baseCost :: ISLLine -> Int
baseCost = \case
  LineCut{} -> line_cut_cost
  PointCut{} -> point_cut_cost
  Color{} -> color_cost
  Swap{} -> 3
  Merge{} -> 1

lineCost :: Block -> ISLLine -> Int -> Integer
lineCost block0 move targetSize = round (fromIntegral (baseCost move * blockArea block0) / fromIntegral targetSize :: Double)

imgLineCost :: Img -> Img -> Int -> Integer
imgLineCost whole region base = round (fromIntegral (base * imgSize whole) / fromIntegral (imgSize region) :: Double)

cost :: Blocks -> ISL -> Integer
cost initialBlocks = snd . foldl' f (initialBlocks, 0)
  where
    -- TODO: might we ever remove block0??
    block0 = lookupBlock [0] initialBlocks
    f (blocks, c) move =
      ( blockEffect move blocks
      , c + lineCost block0 move (targetSize move blocks)
      )

targetSize :: ISLLine -> Blocks -> Int
targetSize isl blocks = case isl of
  LineCut b _ _ -> f b
  PointCut b _ -> f b
  Color b _ -> f b
  Swap b0 _ -> f b0
  Merge b0 b1 -> max (f b0) (f b1)
  where
    f = blockArea . flip lookupBlock blocks

similarity :: Img -> Img -> Integer
similarity a b = round $ 0.005 * go 0 componentDiffs
  where
    go total v | V.length v == 0 = total
    go total v = go (total + sqrt (V.sum (V.take 4 v))) (V.drop 4 v)
    componentDiffs = V.zipWith componentDifference (imageData a) (imageData b)

colorDiff :: RGBA -> RGBA -> Double
colorDiff (PixelRGBA8 r0 g0 b0 a0) (PixelRGBA8 r1 g1 b1 a1) =
  sqrt (
    componentDifference r0 r1 +
    componentDifference g0 g1 +
    componentDifference b0 b1 +
    componentDifference a0 a1
  )

componentDifference :: Word8 -> Word8 -> Double
componentDifference c c' = (fromIntegral c - fromIntegral c') ** 2

similarityWithAverage :: Img -> Integer
similarityWithAverage img = similarityWithSolidColor img (averageColour img)

similarityWithSolidColor :: Img -> RGBA -> Integer
similarityWithSolidColor img (PixelRGBA8 r g b a) =
 round $ 0.005 * go 0 (imageData img)
  where
    cvec = V.fromList [r, g, b, a]
    go total v | V.length v == 0 = total
    go total v = go (total + sqrt (V.sum (diffs (V.take 4 v)))) (V.drop 4 v)
    diffs = V.zipWith componentDifference cvec
