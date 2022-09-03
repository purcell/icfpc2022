{-# LANGUAGE LambdaCase #-}
module Draw where

import Blocks
import Types
import Codec.Picture.Types (writePixel, readPixel)
import Codec.Picture.Drawing (fillRectangle, withMutableImage)
import Data.Foldable (foldlM, for_)
import Data.Functor (void)

draw :: Blocks -> ISL -> IO Img
draw blocks0 isl =
  withMutableImage w h (PixelRGBA8 255 255 255 255) $ \img ->
    void $ foldlM (step img) blocks0 isl
  where
    w = blockWidth block0
    h = blockHeight block0
    block0 = lookupBlock [0] blocks0
    step img blocks move = do
      drawOne move blocks img
      pure (blockEffect move blocks)
    drawOne (Color b c) blocks img = color (lookupBlock b blocks) c img
    drawOne (Swap b0 b1) blocks img = swap (lookupBlock b0 blocks) (lookupBlock b1 blocks) img
    drawOne _ _ _ = pure ()
    color (Rect (x0, y0) (x1, y1)) c img = fillRectangle img x0 (400 - y1) (x1 - 1) (399 - y0) c
    swap (Rect (x0, y0) (x1, y1)) (Rect (tx, ty) _) img =
      for_ [x0..x1-1] $ \x ->
        for_ [y0..y1-1] $ \y -> do
          let x' = x - x0 + tx
              y' = y - y0 + ty
          px <- readPixel img x (399 - y)
          px' <- readPixel img x' (399 - y')
          writePixel img x (399 - y) px'
          writePixel img x' (399 - y') px
