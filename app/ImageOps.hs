module ImageOps where

import Codec.Picture (imagePixels, pixelAt, palettize, PaletteOptions (..), PaletteCreationMethod (..), generateImage, imageWidth, imageHeight, mixWith)
import Codec.Picture.Types (promotePixel, dropTransparency)
import Data.Monoid (Sum(..))
import Lens.Micro (over)
import Lens.Micro.Internal (foldMapOf)

import Types hiding (pixelAt)

averageColour :: Img -> RGBA
averageColour = avg . foldMapOf imagePixels (\(PixelRGBA8 r g b _) -> C (fromIntegral r) (fromIntegral g) (fromIntegral b) 1)
  where
    avg (C r g b n) = let f c = fromIntegral (c `div` n) in
      PixelRGBA8 (f r) (f g) (f b) 255

averageColour' :: Img -> RGBA
averageColour' img
  = promotePixel
  $ pixelAt (snd $ palettize (PaletteOptions MedianMeanCut False 1)
  $ over imagePixels dropTransparency img) 0 0

avg2 :: RGBA -> RGBA -> RGBA
avg2 = mixWith (\_ ca cb -> fromIntegral $ (fromIntegral ca + fromIntegral cb :: Int) `div` 2)

region :: Img -> Point -> Point -> Img
region i (x0,y0) (x1,y1) = generateImage (\x y -> pixelAt i (x + x0) (h - 1 - (y + y0))) (x1 - x0) (y1 - y0)
  where h = imageHeight i

flatColorImg :: Int -> Int -> RGBA -> Img
flatColorImg w h c = generateImage (\_ _ -> c) w h

filledWith :: Img -> RGBA -> Img
filledWith img colour = generateImage (\_ _ -> colour) (imageWidth img) (imageHeight img)

imgSize :: Img -> Int
imgSize img = imageHeight img * imageWidth img

data ColorSum = C !Int !Int !Int !Int
instance Semigroup ColorSum where
  C r0 g0 b0 n0 <> C r1 g1 b1 n1 = C (r0 + r1) (g0 + g1) (b0 + b1) (n0 + n1)
instance Monoid ColorSum where
  mempty = C 0 0 0 0

averageRegionColour :: Img -> Point -> Point -> RGBA
averageRegionColour img (x0,y0) (x1,y1) =
    avg $ foldMap (\(PixelRGBA8 r g b _) -> C (fromIntegral r) (fromIntegral g) (fromIntegral b) 1)
      [ pixelAt img x (imageHeight img - 1 - y) | x <- [x0 .. x1 - 1], y <- [y0 .. y1 - 1]]
  where
    avg (C r g b n) = let f c = fromIntegral (c `div` n) in
      PixelRGBA8 (f r) (f g) (f b) 255

blit :: Img -> Point -> Point -> Img -> Img
blit bg (x0, y0) (x1, y1) ov = generateImage blitter (imageWidth bg) (imageHeight bg)
  where
    ymax = imageHeight bg - 1
    -- TODO maths is all wrong here
    blitter x y | x >= x0 && x < x1 && y <= ymax - y0 && y < ymax - y1 = pixelAt ov (x - x0) (ymax -  y)
    blitter x y = pixelAt bg x y
