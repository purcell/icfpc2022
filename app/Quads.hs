{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Quads where

import Types hiding (bl, tr)
import Codec.Picture (mixWith, imageWidth, imageHeight)

data Quad
  = Leaf
    { color :: RGBA
    , size :: Int
    }
  | Quad
    { cutPoint :: !Point
    , bl :: !Quad
    , br :: !Quad
    , tr :: !Quad
    , tl :: !Quad
    , size :: !Int
    , qTotalR :: !Int
    , qTotalG :: !Int
    , qTotalB :: !Int
    , qTotalA :: !Int
    }

mkLeaf2 :: RGBA -> RGBA -> Quad
mkLeaf2 a b = Leaf (mixWith (\_ ca cb -> fromIntegral $ (fromIntegral ca + fromIntegral cb :: Int) `div` 2) a b) 2

mkQuad :: Point -> Quad -> Quad -> Quad -> Quad -> Quad
mkQuad p bl br tr tl = Quad p bl br tr tl (sm size) (sm totalR) (sm totalG) (sm totalB) (sm totalA)
  where
    sm f = f bl + f br + f tr + f tl
    totalR = \case Leaf (PixelRGBA8 r _ _ _) s -> s * fromIntegral r; Quad{qTotalR} -> qTotalR
    totalG = \case Leaf (PixelRGBA8 _ g _ _) s -> s * fromIntegral g; Quad{qTotalG} -> qTotalG
    totalB = \case Leaf (PixelRGBA8 _ _ b _) s -> s * fromIntegral b; Quad{qTotalB} -> qTotalB
    totalA = \case Leaf (PixelRGBA8 _ _ _ a) s -> s * fromIntegral a; Quad{qTotalA} -> qTotalA

average :: Quad -> RGBA
average (Leaf c _) = c
average Quad{size,qTotalR,qTotalG,qTotalB,qTotalA} = PixelRGBA8 (f qTotalR) (f qTotalG) (f qTotalB) (f qTotalA)
  where
    f c = fromIntegral (fromIntegral c `div` size) -- round (fromIntegral c / fromIntegral size :: Double)

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
      (fromImage' img (x0, y0) (x, y))
      (fromImage' img (x, y0) (x1, y))
      (fromImage' img (x, y) (x1, y1))
      (fromImage' img (x0, y) (x, y1))

powOf2 :: Int -> Int
powOf2 w = 2 ^ (floor (log (fromIntegral w :: Double) / log 2) :: Int)

toISL :: BlockId -> Quad -> ISL
toISL blockId q@Leaf{}                = [Color blockId (average q)]
toISL blockId q | length blockId > 2 = [Color blockId (average q)]
toISL blockId (Quad{cutPoint,bl,br,tr,tl}) =
  [PointCut blockId cutPoint]
  ++ toISL (blockId ++ [0]) bl
  ++ toISL (blockId ++ [1]) br
  ++ toISL (blockId ++ [2]) tr
  ++ toISL (blockId ++ [3]) tl