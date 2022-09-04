{-# LANGUAGE ScopedTypeVariables #-}
module Draw where

import Blocks
import Types
import ImageOps

draw :: Int -> Int -> Blocks -> ISL -> Img
draw w h blocks0 isl =
  let
    bg = flatColorImg w h (PixelRGBA8 255 255 255 255)
    blocks' :: Blocks = foldl (flip blockEffect) blocks0 isl
  in
    foldl (\im (Rect bl tr i) -> blit im bl tr i) bg (Blocks.toList blocks')
