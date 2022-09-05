module Manually where

import Types

manually :: [(Int, ISL)]
manually = [(1, prob1)]

prob1 :: ISL
prob1 =
  [ Color [0] (PixelRGBA8 0 74 173 255)
  , PointCut [0] (358, 43)
  , Color [0,3] (PixelRGBA8 255 255 255 255)
  , PointCut [0,3] (199, 202)
  , Color [0,3,0] (PixelRGBA8 0 0 0 255)
  , Color [0,3,2] (PixelRGBA8 0 0 0 255)
  , Merge [0,3,0] [0,3,3]
  , Merge [0,3,1] [0,3,2]
  , LineCut [1] X 40
  , LineCut [1,1] X 80
  , LineCut [1,1,1] X 120
  , LineCut [1,1,1,1] X 159
  , LineCut [2] X 238
  , LineCut [2,1] X 278
  , LineCut [2,1,1] X 318
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
  , LineCut [10] Y 83
  , LineCut [10,1] Y 123
  , LineCut [10,1,1] Y 162
  , LineCut [10,1,1,1] Y 202
  , LineCut [10,1,1,1,1] Y 241
  , LineCut [10,1,1,1,1,1] Y 281
  , LineCut [10,1,1,1,1,1,1] Y 321
  , LineCut [10,1,1,1,1,1,1,1] Y 360
  , Swap [10,0] [10,1,1,1,1,1,0]
  , Swap [10,1,1,0] [10,1,1,1,1,1,1,1,0]
  , LineCut [10,1,1,1,1,1,0] X 318
  , Color [10,1,1,1,1,1,0,1] (PixelRGBA8 0 74 173 255)
  ]