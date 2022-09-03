{-# LANGUAGE TypeApplications #-}
module Main where

import Codec.Picture (readImage, convertRGBA8, imagePixels, pixelAt, palettize, PaletteOptions (..), PaletteCreationMethod (..), PixelRGB8 (PixelRGB8), writePng)
import Codec.Picture.Types (promotePixel, dropTransparency)
import Data.Foldable ( for_ )
import Data.Monoid (Sum(..))
import Lens.Micro (over)
import Lens.Micro.Internal (foldMapOf)

import Types
import Cost (cost, similarity)
import ISL (serialize)
import Draw (draw)
import qualified Quads

main :: IO ()
main = for_ [1..25] $ \i -> do
  img <- load i
  let prog = Quads.toISL [0] $ Quads.fromImage img
  img' <- draw prog
  let score = cost prog + similarity img img'
  save i score prog img'
  putStr $ show i ++ " Cost: " ++ show score ++ "\n" ++ serialize prog

load :: Int -> IO Img
load i = do
  res <- readImage ("problems/" ++ show i ++ ".png")
  either error (pure . convertRGBA8) res

save :: Int -> Integer -> ISL -> Img -> IO ()
save i score isl img = do
  writeFile ("solutions/" ++ show i ++ ".score") (show score)
  writeFile ("solutions/" ++ show i ++ ".isl") (serialize isl)
  writePng ("solutions/" ++ show i ++ ".png") img

average :: Img -> RGBA
average = avg . foldMapOf imagePixels (\(PixelRGBA8 r g b a) -> ((s r, s g, s b, s a), Sum 1))
  where
    s = Sum . toInteger
    avg ((Sum r, Sum g, Sum b, Sum a), Sum n) = let f c = fromInteger (c `div` n) in
      PixelRGBA8 (f r) (f g) (f b) (f a)

average' :: Img -> RGBA
average' img
  = promotePixel
  $ pixelAt (snd $ palettize (PaletteOptions MedianMeanCut False 1)
  $ over imagePixels dropTransparency img) 0 0

average'' :: Img -> RGBA
average'' = Quads.average . Quads.fromImage