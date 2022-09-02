{-# LANGUAGE LambdaCase #-}
module Draw where

import Blocks
import Types
import Codec.Picture.Types (createMutableImage, freezeImage)
import Codec.Picture.Drawing (fillRectangle, withMutableImage)
import Data.Foldable (foldrM)
import Data.Functor (void)

draw :: ISL -> IO Img
draw isl =
  withMutableImage 400 400 (PixelRGBA8 255 255 255 255) $ \img ->
    void $ foldrM (step img) [block0] isl
  where
    step img move blocks = do
      drawOne move blocks img
      pure (blockEffect move blocks)
    drawOne (Color b c) blocks img = color (lookupBlock b blocks) c img
    drawOne _ _ _ = pure ()
    color (SimpleBlock (Rect (x0, y0) (x1, y1))) c img = fillRectangle img x0 y0 x1 y1 c