{-# LANGUAGE TypeApplications #-}
module Main where

import Codec.Picture (readImage, convertRGBA8, imagePixels, pixelAt, palettize, PaletteOptions (..), PaletteCreationMethod (..), PixelRGB8 (PixelRGB8), writePng)
import Codec.Picture.Types (promotePixel)
import Data.Foldable ( for_ )
import Data.Monoid (Sum(..))
import Lens.Micro (over)
import Lens.Micro.Internal (foldMapOf)

import Types
import Cost (cost, similarity)
import ISL (serialize)
import Draw (draw)

main :: IO ()
main = for_ [1..15] $ \i -> do
  img <- load i
  let prog = [Color [0] (average img)]
  img' <- draw prog
  save i prog img'
  putStr $ show i ++ " Cost: " ++ show (cost prog + similarity img img') ++ "\n" ++ serialize prog

load :: Int -> IO Img
load i = do
  res <- readImage ("problems/" ++ show i ++ ".png")
  either error (pure . convertRGBA8) res

save :: Int -> ISL -> Img -> IO ()
save i isl img = do
  writeFile ("solutions/" ++ show i ++ ".isl") (serialize isl)
  writePng ("solutions/" ++ show i ++ ".png") img

average :: Img -> RGBA
average = avg . foldMapOf imagePixels (\(PixelRGBA8 r g b a) -> ((s r, s g, s b, s a), Sum 1))
  where
    s = Sum . toInteger
    avg ((Sum r, Sum g, Sum b, Sum a), Sum n) = let f c = fromInteger (c `div` n) in
      PixelRGBA8 (f r) (f g) (f b) (f a)

average' :: Img -> RGBA
average' img = promotePixel $ pixelAt (snd $ palettize (PaletteOptions MedianMeanCut False 1) $ over imagePixels dropAlpha img) 0 0
  where
    dropAlpha (PixelRGBA8 r g b _) = PixelRGB8 r g b