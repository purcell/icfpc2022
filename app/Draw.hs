{-# LANGUAGE LambdaCase #-}
module Draw where

import Blocks
import Types
import Codec.Picture.Types (createMutableImage, freezeImage, writePixel, readPixel)
import Codec.Picture.Drawing (fillRectangle, withMutableImage)
import Data.Foldable (foldlM, for_)
import Data.Functor (void)

draw :: ISL -> IO Img
draw isl =
  withMutableImage 400 400 (PixelRGBA8 255 255 255 255) $ \img ->
    void $ foldlM (step img) [block0] isl
  where
    step img blocks move = do
      drawOne move blocks img
      pure (blockEffect move blocks)
    drawOne (Color b c) blocks img = color (lookupBlock b blocks) c img
    drawOne (Swap b0 b1) blocks img = swap (lookupBlock b0 blocks) (lookupBlock b1 blocks) img
    drawOne _ _ _ = pure ()
    color (SimpleBlock (Rect (x0, y0) (x1, y1))) c img = fillRectangle img x0 (400 - y1) (x1 - 1) (399 - y0) c
    swap (SimpleBlock (Rect (x0, y0) (x1, y1))) (SimpleBlock (Rect (tx, ty) _)) img =
      for_ [x0..x1-1] $ \x ->
        for_ [y0..y1-1] $ \y -> do
          let x' = x - x0 + tx
              y' = y - y0 + ty
          px <- readPixel img x (399 - y)
          px' <- readPixel img x' (399 - y')
          writePixel img x (399 - y) px'
          writePixel img x' (399 - y') px