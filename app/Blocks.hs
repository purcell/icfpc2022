{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Blocks where

import Types
import Lens.Micro (Traversal', ix, over, (.~), (&), _2, at)
import Lens.Micro.GHC ()
import qualified Data.Map.Strict as Map

fromInitialLayout :: InitialLayout -> Blocks
fromInitialLayout InitialLayout{layoutW,layoutH,layoutBlocks} =
  (length layoutBlocks,
   Map.fromList ((\(InitialBlock{iBlockBL,iBlockTR,iBlockID,iBlockColor}) ->
                   (iBlockID, Rect iBlockBL iBlockTR iBlockColor)) <$> layoutBlocks))

toList :: Blocks -> [Block]
toList (_, bs) = Map.elems bs

-- >>> lookupBlock [0] blocks0
-- SimpleBlock (Rect {bl = (0,0), tr = (400,400)})
lookupBlock :: BlockId -> Blocks -> Block
lookupBlock bid = (Map.! bid) . snd

blockArea :: Block -> Int
blockArea b = blockHeight b * blockWidth b

blockWidth :: Block -> Int
blockWidth (Rect (x0, _) (x1, _) _) = x1 - x0

blockHeight :: Block -> Int
blockHeight (Rect (_, y0) (_, y1) _) = y1 - y0

blockAt :: BlockId -> Traversal' Blocks Block
blockAt bid = _2 . ix bid

destroy :: BlockId -> Blocks -> Blocks
destroy bid = (_2 . at bid) .~ Nothing

append :: Block -> Blocks -> Blocks
append b (n, m) = (n + 1, Map.insert [n] b m)

replace :: BlockId -> (Block -> [Block]) -> Blocks -> Blocks
replace bid f blocks =
  destroy bid
  $ flip (foldr (\(i, b) -> over _2 (Map.insert (bid ++ [i]) b))) (zip [0..] (f $ lookupBlock bid blocks))
  $ blocks

blockEffect :: ISLLine -> Blocks -> Blocks
blockEffect = \case
  LineCut b o l -> replace b (lineCut o l)
  PointCut b p -> replace b (pointCut p)
  Color b c -> over _2 (Map.update (\r -> Just (r { bcolor = c }) ) b)
  Swap b0 b1 -> swapBlocks b0 b1
  Merge b0 b1 -> \blocks -> destroy b0 . destroy b1 . append (merge (lookupBlock b0 blocks) (lookupBlock b1 blocks)) $ blocks

-- >>> swap [0,0] [0,1] [lineCut X 100 block0]
-- [ComplexBlock [SimpleBlock (Rect {bl = (100,0), tr = (400,400)}),SimpleBlock (Rect {bl = (0,0), tr = (100,400)})]]
swapBlocks :: BlockId -> BlockId -> Blocks -> Blocks
swapBlocks a b blocks =
  let blockA = lookupBlock a blocks
      blockB = lookupBlock b blocks
  in blocks & blockAt a .~ blockB & blockAt b .~ blockA

-- >>> lookupBlock [0,1] [lineCut X 100 block0]
-- SimpleBlock (Rect {bl = (100,0), tr = (400,400)})
lineCut :: Orientation -> Int -> Block -> [Block]
lineCut X x (Rect (x0, y0) (x1, y1) c) =
  [Rect (x0, y0) (x, y1) c, Rect (x, y0) (x1, y1) c]
lineCut Y y (Rect (x0, y0) (x1, y1) c) =
  [Rect (x0, y0) (x1, y) c, Rect (x0, y) (x1, y1) c]

-- >>> lookupBlock [0,1] [pointCut (100, 200) block0]
-- SimpleBlock (Rect {bl = (100,0), tr = (400,200)})
pointCut :: Point -> Block -> [Block]
pointCut (x, y) (Rect (x0, y0) (x1, y1) c) =
  [ Rect (x0, y0) (x, y) c
  , Rect (x, y0) (x1, y) c
  , Rect (x, y) (x1, y1) c
  , Rect (x0, y) (x, y1) c
  ]

color :: RGBA -> Block -> [Block]
color c r = [r { bcolor = c }]

merge :: Block -> Block -> Block
merge (Rect (ax0, ay0) (ax1, ay1) c1) (Rect (bx0, by0) (bx1, by1) c2) =
  if ax0 == bx0 && ax1 == bx1
    then if ay1 == by0
      then Rect (ax0, ay0) (bx1, by1) c2
      else if ay0 == by1
        then Rect (bx0, by0) (ax1, ay1) c2
        else error "merge: blocks do not share an edge"
    else if ay0 == by0 && ay1 == by1
      then if ax1 == bx0
        then Rect (ax0, ay0) (bx1, by1) c2
        else if ax0 == bx1
          then Rect (bx0, by0) (ax1, ay1) c2
          else error "merge: blocks do not share an edge"
      else error "merge: blocks do not share an edge"
