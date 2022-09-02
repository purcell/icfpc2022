{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Blocks where

import Types
import Lens.Micro (Traversal', ix, over)

shape0 :: Shape
shape0 = Rect (0, 0) (399, 399)

block0 :: Block
block0 = SimpleBlock shape0

-- >>> lookupBlock [0] [block0]
-- SimpleBlock (Rect {bl = (0,0), tr = (399,399)})
lookupBlock :: BlockId -> [Block] -> Block
lookupBlock [] _ = error "unexpected empty block id"
lookupBlock (i:blockId) bs = case (bs !! i, blockId) of
  (block, []) -> block
  (ComplexBlock bs, _) -> lookupBlock blockId bs

blockAt :: BlockId -> Traversal' [Block] Block
blockAt [i] f bs = ix i f bs
blockAt (i:id) f bs = ix i f' bs
  where
    f' (ComplexBlock bs') = ComplexBlock <$> blockAt id f bs'

blockEffect :: ISLLine -> [Block] -> [Block]
blockEffect = \case
  LineCut b o l -> over (blockAt b) (lineCut o l)
  PointCut b p -> over (blockAt b) (pointCut p)
  Color b c -> id
  Swap b0 b1 -> id
  Merge b0 b1 -> \blocks -> blocks ++ [merge (lookupBlock b0 blocks) (lookupBlock b1 blocks)]

-- >>> lookupBlock [0,1] [lineCut X 100 block0]
-- SimpleBlock (Rect {bl = (100,0), tr = (399,399)})
lineCut :: Orientation -> Int -> Block -> Block
lineCut X x (SimpleBlock (Rect (x0, y0) (x1, y1))) =
  ComplexBlock [SimpleBlock (Rect (x0, y0) (x, y1)), SimpleBlock (Rect (x, y0) (x1, y1))]
lineCut Y y (SimpleBlock (Rect (x0, y0) (x1, y1))) =
  ComplexBlock [SimpleBlock (Rect (x0, y0) (x1, y)), SimpleBlock (Rect (x0, y) (x1, y1))]
lineCut _ _ (ComplexBlock _) = error "lineCut: unexpected complex block"

-- >>> lookupBlock [0,1] [pointCut (100, 200) block0]
-- SimpleBlock (Rect {bl = (100,0), tr = (399,200)})
pointCut :: Point -> Block -> Block
pointCut (x, y) (SimpleBlock (Rect (x0, y0) (x1, y1))) =
  ComplexBlock
    [ SimpleBlock (Rect (x0, y0) (x, y))
    , SimpleBlock (Rect (x, y0) (x1, y))
    , SimpleBlock (Rect (x, y) (x1, y1))
    , SimpleBlock (Rect (x0, y) (x, y1))
    ]
pointCut _ (ComplexBlock _) = error "pointCut: unexpected complex block"

merge :: Block -> Block -> Block
merge (SimpleBlock (Rect (ax0, ay0) (ax1, ay1))) (SimpleBlock (Rect (bx0, by0) (bx1, by1))) =
  if ax0 == bx0 && ax1 == bx1
    then if ay1 == by0
      then SimpleBlock (Rect (ax0, ay0) (bx1, by1))
      else if ay0 == by1
        then SimpleBlock (Rect (bx0, by0) (ax1, ay1))
        else error "merge: blocks do not share an edge"
    else if ay0 == by0 && ay1 == by1
      then if ax1 == bx0
        then SimpleBlock (Rect (ax0, ay0) (bx1, by1))
        else if ax0 == bx1
          then SimpleBlock (Rect (bx0, by0) (ax1, ay1))
          else error "merge: blocks do not share an edge"
      else error "merge: blocks do not share an edge"
