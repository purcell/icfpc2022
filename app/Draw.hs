{-# LANGUAGE ScopedTypeVariables #-}
module Draw where

import Blocks
import Types
import ImageOps
import Codec.Picture (imageWidth, imageHeight)
import Codec.Picture.Drawing (withMutableImage)
import Data.Foldable (for_)

draw :: Int -> Int -> Blocks -> ISL -> IO Img
draw w h blocks0 isl =
  let
    blocks' = Blocks.toList $ blockEffects isl blocks0
  in
    withMutableImage w h (PixelRGBA8 255 255 255 255) $ \im ->
      for_ blocks' $ \(Rect (x0, y0) (x1, y1) bim) ->
        for_ [(x, y) | x <- [0..imageWidth bim - 1], y <- [0..imageHeight bim - 1]] $ \(x, y) ->
          writePixel im (x + x0, y + y0) $ pixelAt bim (x, y)
