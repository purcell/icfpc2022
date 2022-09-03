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
  let prog = if i == 1 then prob1 else Quads.toISL [0] $ Quads.fromImage img
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

prob1 :: ISL
prob1 =
  [ Color [0] (PixelRGBA8 0 74 173 255)
  , PointCut [0] (360, 40)
  , Color [0,3] (PixelRGBA8 255 255 255 255)
  , PointCut [0,3] (200, 200)
  , Color [0,3,0] (PixelRGBA8 0 0 0 255)
  , Color [0,3,2] (PixelRGBA8 0 0 0 255)
  , Merge [0,3,0] [0,3,3]
  , Merge [0,3,1] [0,3,2]
  , LineCut [1] X 40
  , LineCut [1,1] X 80
  , LineCut [1,1,1] X 120
  , LineCut [1,1,1,1] X 160
  , LineCut [2] X 240
  , LineCut [2,1] X 280
  , LineCut [2,1,1] X 320
  , Swap [1,1,0] [2,1,0]
  , Swap [1,1,1,1,0] [2,1,1,1]
  , Merge [1,0] [2,1,0]
  , Merge [3] [1,1,1,0]
  , Merge [4] [2,1,1,1]
  , Merge [5] [1,1,1,1,1]
  , Merge [6] [2,0]
  , Merge [7] [1,1,0]
  , Merge [8] [2,1,1,0]
  , Merge [9] [1,1,1,1,0]
  , LineCut [10] Y 80
  , LineCut [10,1] Y 120
  , LineCut [10,1,1] Y 160
  , LineCut [10,1,1,1] Y 200
  , LineCut [10,1,1,1,1] Y 240
  , LineCut [10,1,1,1,1,1] Y 280
  , LineCut [10,1,1,1,1,1,1] Y 320
  , LineCut [10,1,1,1,1,1,1,1] Y 360
  , Swap [10,0] [10,1,1,1,1,1,0]
  , Swap [10,1,1,0] [10,1,1,1,1,1,1,1,0]
  ]