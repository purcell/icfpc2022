{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Blocks where

import Types
import Lens.Micro (Traversal', ix, over, (.~), (&), _2, at)
import Lens.Micro.GHC ()
import qualified Data.Map.Strict as Map

block0 :: Block
block0 = Rect (0, 0) (400, 400)

blocks0 :: Blocks
blocks0 = (1, Map.singleton [0] block0)

-- >>> lookupBlock [0] blocks0
-- SimpleBlock (Rect {bl = (0,0), tr = (400,400)})
lookupBlock :: BlockId -> Blocks -> Block
lookupBlock id = (Map.! id) . snd

blockAt :: BlockId -> Traversal' Blocks Block
blockAt id = _2 . ix id

destroy :: BlockId -> Blocks -> Blocks
destroy id = (_2 . at id) .~ Nothing

append :: Block -> Blocks -> Blocks
append b (n, m) = (n + 1, Map.insert [n] b m)

replace :: BlockId -> (Block -> [Block]) -> Blocks -> Blocks
replace id f blocks =
  destroy id
  $ flip (foldr (\(i, b) -> over _2 (Map.insert (id ++ [i]) b))) (zip [0..] (f $ lookupBlock id blocks))
  $ blocks

blockEffect :: ISLLine -> Blocks -> Blocks
blockEffect = \case
  LineCut b o l -> replace b (lineCut o l)
  PointCut b p -> replace b (pointCut p)
  Color b c -> id
  Swap b0 b1 -> swap b0 b1
  Merge b0 b1 -> \blocks -> destroy b0 . destroy b1 . append (merge (lookupBlock b0 blocks) (lookupBlock b1 blocks)) $ blocks

-- >>> swap [0,0] [0,1] [lineCut X 100 block0]
-- [ComplexBlock [SimpleBlock (Rect {bl = (100,0), tr = (400,400)}),SimpleBlock (Rect {bl = (0,0), tr = (100,400)})]]
swap :: BlockId -> BlockId -> Blocks -> Blocks
swap b0 b1 blocks =
  let block0 = lookupBlock b0 blocks
      block1 = lookupBlock b1 blocks
  in blocks & blockAt b0 .~ block1 & blockAt b1 .~ block0

-- >>> lookupBlock [0,1] [lineCut X 100 block0]
-- SimpleBlock (Rect {bl = (100,0), tr = (400,400)})
lineCut :: Orientation -> Int -> Block -> [Block]
lineCut X x (Rect (x0, y0) (x1, y1)) =
  [Rect (x0, y0) (x, y1), Rect (x, y0) (x1, y1)]
lineCut Y y (Rect (x0, y0) (x1, y1)) =
  [Rect (x0, y0) (x1, y), Rect (x0, y) (x1, y1)]

-- >>> lookupBlock [0,1] [pointCut (100, 200) block0]
-- SimpleBlock (Rect {bl = (100,0), tr = (400,200)})
pointCut :: Point -> Block -> [Block]
pointCut (x, y) (Rect (x0, y0) (x1, y1)) =
  [ Rect (x0, y0) (x, y)
  , Rect (x, y0) (x1, y)
  , Rect (x, y) (x1, y1)
  , Rect (x0, y) (x, y1)
  ]

merge :: Block -> Block -> Block
merge (Rect (ax0, ay0) (ax1, ay1)) (Rect (bx0, by0) (bx1, by1)) =
  if ax0 == bx0 && ax1 == bx1
    then if ay1 == by0
      then Rect (ax0, ay0) (bx1, by1)
      else if ay0 == by1
        then Rect (bx0, by0) (ax1, ay1)
        else error "merge: blocks do not share an edge"
    else if ay0 == by0 && ay1 == by1
      then if ax1 == bx0
        then Rect (ax0, ay0) (bx1, by1)
        else if ax0 == bx1
          then Rect (bx0, by0) (ax1, ay1)
          else error "merge: blocks do not share an edge"
      else error "merge: blocks do not share an edge"
