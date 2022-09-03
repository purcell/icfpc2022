module ImageOps where

import Codec.Picture (convertRGBA8, imagePixels, pixelAt, palettize, PaletteOptions (..), PaletteCreationMethod (..), PixelRGB8 (PixelRGB8), generateImage, imageWidth, imageHeight, mixWith)
import Codec.Picture.Types (promotePixel, dropTransparency)
import Data.Monoid (Sum(..))
import Lens.Micro (over)
import Lens.Micro.Internal (foldMapOf)

import Types hiding (pixelAt)

averageColour :: Img -> RGBA
averageColour = avg . foldMapOf imagePixels (\(PixelRGBA8 r g b a) -> ((s r, s g, s b, s a), Sum 1))
  where
    s = Sum . toInteger
    avg ((Sum r, Sum g, Sum b, Sum a), Sum n) = let f c = fromInteger (c `div` n) in
      PixelRGBA8 (f r) (f g) (f b) (f a)

averageColour' :: Img -> RGBA
averageColour' img
  = promotePixel
  $ pixelAt (snd $ palettize (PaletteOptions MedianMeanCut False 1)
  $ over imagePixels dropTransparency img) 0 0

avg2 :: RGBA -> RGBA -> RGBA
avg2 = mixWith (\_ ca cb -> fromIntegral $ (fromIntegral ca + fromIntegral cb :: Int) `div` 2)

region :: Img -> Point -> Point -> Img
region i (x0,y0) (x1,y1) = generateImage (\x y -> pixelAt i (x + x0) (399 - (y + y0))) (x1 - x0) (y1 - y0)

filledWith :: Img -> RGBA -> Img
filledWith img colour = generateImage (\_ _ -> colour) (imageWidth img) (imageHeight img)
