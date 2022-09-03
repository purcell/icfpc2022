{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Quads where

import Types hiding (bl, tr)
import Codec.Picture (mixWith, imageWidth, imageHeight, generateImage)
import ImageOps

data Quad
  = Leaf
    { color :: RGBA
    , size :: Int
    }
  | Quad
    { cutPoint :: !Point
    , averageColor :: !RGBA
    , bl :: !Quad
    , br :: !Quad
    , tr :: !Quad
    , tl :: !Quad
    , size :: !Int
    }

mkLeaf2 :: RGBA -> RGBA -> Quad
mkLeaf2 a b = Leaf (mixWith (\_ ca cb -> fromIntegral $ (fromIntegral ca + fromIntegral cb :: Int) `div` 2) a b) 2

mkQuad :: Point -> RGBA -> Quad -> Quad -> Quad -> Quad -> Quad
mkQuad p c bl br tr tl = Quad p c bl br tr tl (s size)
  where
    s f = f bl + f br + f tr + f tl

fromImage :: Img -> Quad
fromImage img = fromImage' img (0, 0) (imageWidth img, imageHeight img)

fromImage' :: Img -> Point -> Point -> Quad
fromImage' img (x0,y0) (x1,y1) = case (x1 - x0, y1 - y0) of
  (0, _) -> error "zero width"
  (_, 0) -> error "zero height"
  (1, 1) -> Leaf (pixelAt img (x0, y0)) 1
  (1, 2) -> mkLeaf2 (pixelAt img (x0, y0)) (pixelAt img (x0, y0 + 1))
  (2, 1) -> mkLeaf2 (pixelAt img (x0, y0)) (pixelAt img (x0 + 1, y0))
  (w, h) -> let x = x0 + w `div` 2; y = y0 + h `div` 2 in
    if (x0 == x || x1 == x) then error $ show (x0, x1, x) else
    mkQuad
      (x, y)
      (averageColour (region img (x0,y0) (x1,y1)))
      (fromImage' img (x0, y0) (x, y))
      (fromImage' img (x, y0) (x1, y))
      (fromImage' img (x, y) (x1, y1))
      (fromImage' img (x0, y) (x, y1))

powOf2 :: Int -> Int
powOf2 w = 2 ^ (floor (log (fromIntegral w :: Double) / log 2) :: Int)

toISL :: BlockId -> Quad -> ISL
toISL blockId q | length blockId > 2 = [Color blockId (averageColor q)]
toISL blockId (Quad{cutPoint,bl,br,tr,tl}) =
  [PointCut blockId cutPoint]
  ++ toISL (blockId ++ [0]) bl
  ++ toISL (blockId ++ [1]) br
  ++ toISL (blockId ++ [2]) tr
  ++ toISL (blockId ++ [3]) tl
