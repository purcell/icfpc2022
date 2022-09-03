{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Quads where

import Types hiding (bl, tr)
import Codec.Picture (mixWith, imageWidth, imageHeight, generateImage)
import ImageOps
import Cost (componentDifference)

fromImage :: Img -> ISL
fromImage img = toISL img (0, 0) (imageWidth img, imageHeight img) [0]

powOf2 :: Int -> Int
powOf2 w = 2 ^ (floor (log (fromIntegral w :: Double) / log 2) :: Int)

toISL :: Img -> Point -> Point -> BlockId -> ISL
toISL img (x0,y0) (x1,y1) blockId =
  let w = x1 - x0
      h = y1 - y0
      x = x0 + w `div` 2
      y = y0 + h `div` 2
      bl = region img (x0, y0) (x, y)
      br = region img (x, y0) (x1, y)
      tl = region img (x, y) (x1, y1)
      tr = region img (x0, y) (x, y1)
  in if similar (length blockId) (averageColour bl) (averageColour br) (averageColour tl) (averageColour tr)
    then
      [Color blockId (averageColour (region img (x0,y0) (x1,y1)))]
    else
      [PointCut blockId (x, y)]
      ++ toISL img (x0, y0) (x, y) (blockId ++ [0])
      ++ toISL img (x, y0) (x1, y) (blockId ++ [1])
      ++ toISL img (x, y) (x1, y1) (blockId ++ [2])
      ++ toISL img (x0, y) (x, y1) (blockId ++ [3])

similar :: Int -> RGBA -> RGBA -> RGBA -> RGBA -> Bool
similar f a b c d =
  let avg = avg2 (avg2 a b) (avg2 c d)
      diff (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
        sqrt (componentDifference r1 r2 + componentDifference g1 g2 + componentDifference b1 b2 + componentDifference a1 a2)
  in diff avg a + diff avg b + diff avg c + diff avg d < (16 ^ f) / 20