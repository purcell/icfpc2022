module Manually where

import Types

manually :: [(Int, ISL)]
manually = [(1, prob1)]

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
  , LineCut [10,1,1,1,1,1,0] X 320
  , Color [10,1,1,1,1,1,0,1] (PixelRGBA8 0 74 173 255)
  ]