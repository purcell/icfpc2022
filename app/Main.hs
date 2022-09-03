module Main where

import Codec.Picture (readImage, convertRGBA8, imagePixels, pixelAt, palettize, PaletteOptions (..), PaletteCreationMethod (..), writePng)
import Codec.Picture.Types (promotePixel, dropTransparency)
import Data.Foldable ( for_ )
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(..))
import Lens.Micro (over)
import Lens.Micro.Internal (foldMapOf)

import Types
import Cost (cost, similarity)
import ISL (serialize)
import Draw (draw)
import Manually (manually)
import qualified Quads

main :: IO ()
main = for_ [1..25] $ \i -> do
  img <- load i
  let man = lookup i manually
  let prog = fromMaybe (Quads.toISL [0] $ Quads.fromImage img) man
  -- let prog = [Color [0] (average img)]
  img' <- draw prog
  let cScore = cost prog
  let sScore = similarity img img'
  save i cScore sScore prog img'
  putStr $ show i ++ " Cost: " ++ show (cScore + sScore, cScore, sScore) ++ "\n" -- ++ serialize prog

load :: Int -> IO Img
load i = do
  res <- readImage ("problems/" ++ show i ++ ".png")
  either error (pure . convertRGBA8) res

save :: Int -> Integer -> Integer -> ISL -> Img -> IO ()
save i cScore sScore isl img = do
  writeFile ("solutions/" ++ show i ++ ".score") (unlines $ fmap show [cScore + sScore, cScore, sScore])
  writeFile ("solutions/" ++ show i ++ ".isl") (serialize isl)
  writePng ("solutions/" ++ show i ++ ".png") img
