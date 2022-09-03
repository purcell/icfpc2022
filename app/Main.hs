module Main where

import Codec.Picture (readImage, convertRGBA8, imagePixels, pixelAt, palettize, PaletteOptions (..), PaletteCreationMethod (..), writePng)
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
  -- let prog = [Color [0] (average img)]
  img' <- draw prog
  let score = cost prog + similarity img img'
  save i score prog img'
  putStr $ show i ++ " Cost: " ++ show score ++ "\n" -- ++ serialize prog

load :: Int -> IO Img
load i = do
  res <- readImage ("problems/" ++ show i ++ ".png")
  either error (pure . convertRGBA8) res

save :: Int -> Integer -> ISL -> Img -> IO ()
save i score isl img = do
  writeFile ("solutions/" ++ show i ++ ".score") (show score)
  writeFile ("solutions/" ++ show i ++ ".isl") (serialize isl)
  writePng ("solutions/" ++ show i ++ ".png") img
