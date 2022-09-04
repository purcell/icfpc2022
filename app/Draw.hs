{-# LANGUAGE LambdaCase #-}
module Draw where

import Blocks
import Types
import Codec.Picture.Types (writePixel, readPixel)
import Codec.Picture.Drawing (fillRectangle, withMutableImage)
import Data.Foldable (foldlM, for_)
import Data.Functor (void)

draw :: Int -> Int -> Blocks -> ISL -> IO Img
draw w h blocks0 isl =
  withMutableImage w h (PixelRGBA8 255 255 255 255) $ \img ->
    -- TODO: draw the initial blocks
    void $ foldlM (step img) blocks0 isl
  where
    step img blocks move = do
      drawOne move blocks img
      pure $ blockEffect move blocks
    drawOne (Color b c) blocks img = fill (lookupBlock b blocks) c img
    drawOne (Swap b0 b1) blocks img = swap (lookupBlock b0 blocks) (lookupBlock b1 blocks) img
    drawOne (LineCut _ _ _) _ _ = pure ()
    drawOne (PointCut _ _) _ _ = pure ()
    drawOne (Merge _ _) _ _ = pure ()
    fill (Rect (x0, y0) (x1, y1) _) c img = fillRectangle img x0 (h - y1) (x1 - 1) (h - 1 - y0) c
    swap (Rect (x0, y0) (x1, y1) c1) (Rect (tx, ty) _ c2) img =
      for_ [x0..x1-1] $ \x ->
        for_ [y0..y1-1] $ \y -> do
          let x' = x - x0 + tx
              y' = y - y0 + ty
          px <- readPixel img x (h - 1 - y)
          px' <- readPixel img x' (h - 1 - y')
          writePixel img x (h - 1 - y) px'
          writePixel img x' (h - 1 - y') px
