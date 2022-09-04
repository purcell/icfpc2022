{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Quads where

import Types hiding (bl, tr)
import ImageOps
import qualified Cost
import Data.Foldable (minimumBy, maximumBy)
import Data.Function (on)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Blocks
import Control.Monad.State.Lazy

data BlockState =
  BlockState { target :: Img -- TODO put in reader instead
             , bs :: Blocks
             , isl :: ISL
             , cost :: Integer
             -- Similarities of each block
             , sims :: Map BlockId Integer
             }

type Robo a = State BlockState a

fromImage :: Blocks -> Img -> ISL
fromImage b i = fst $ runRobo i b $ do
  paintAll
  gets isl

runRobo :: Img -> Blocks -> Robo a -> (a, BlockState)
runRobo img blocks a = runState (makeSims >> a) initialState
  where
    initialState = BlockState { target = img, bs = blocks, isl = [], cost = 0, sims = Map.empty }
    makeSims = mapM_ _updateSimilarity (Map.keys (snd blocks))

_totalCost :: Robo Integer
_totalCost = do
  movesCost <- gets cost
  imgSim <- sum . Map.elems <$> gets sims
  pure (movesCost + imgSim)

_updateSimilarity :: BlockId -> Robo ()
_updateSimilarity blockId = do
  (Rect _ _ curBlockImg) <- getBlock blockId
  targetImg <- targetRegion blockId
  modify $ \s ->
    s { sims = Map.insert blockId (Cost.similarity targetImg curBlockImg) (sims s) }

getBlock :: BlockId -> Robo Block
getBlock bid = Blocks.lookupBlock bid <$> gets bs

getTarget :: Robo Img
getTarget = gets target

_lineCost :: ISLLine -> Robo Integer
_lineCost move = do
  canvasArea <- imgSize <$> getTarget
  targetSize  <- Cost.targetSize move <$> gets bs
  pure $ round (fromIntegral (Cost.baseCost move * canvasArea) / fromIntegral targetSize)

targetRegion :: BlockId -> Robo Img
targetRegion blockId = do
  (Rect (x0, y0) (x1, y1) _) <- getBlock blockId
  img <- getTarget
  pure $ region img (x0, y0) (x1, y1)

similarityToTargetRegion :: BlockId -> Robo Integer
similarityToTargetRegion blockId = (Map.! blockId) <$> gets sims

runISLLine :: ISLLine -> Robo ()
runISLLine l = do
  lcost <- _lineCost l
  origBlocks <- gets bs
  let blocks' = Blocks.blockEffect l origBlocks

  modify (\s -> s { isl = isl s ++ [l], bs = blocks', cost = cost s + lcost })

  -- Update block similarities
  forM_ (Map.toList (snd blocks')) $ \(bid, b) -> do
    case Map.lookup bid (snd origBlocks) of
      Just oldb | oldb /= b -> _updateSimilarity bid
      Nothing ->  _updateSimilarity bid
      _ -> pure ()

cheapest :: [ISLLine] -> Robo ()
cheapest ls = do
  s <- get
  let nexts :: [(Integer, BlockState)] = flip runState s . (\l -> runISLLine l >> _totalCost) <$> ls
  let (_, bestNewState) = minimumBy (compare `on` fst) nexts
  modify (const bestNewState)

worstBlock :: Robo BlockId
worstBlock =
  fst . maximumBy (compare `on` snd) . Map.toList <$> gets sims

paintAll :: Robo ()
paintAll = do
  next <- worstBlock
  b <- getBlock next
  s <- similarityToTargetRegion next
  when (s > 100000000 `div` fromIntegral (Blocks.blockArea b)) $ do
    paint next
    paintAll

paint :: BlockId -> Robo ()
paint blockId = do
  (Rect (x0, y0) (x1, y1) _) <- getBlock blockId
  let w = x1 - x0
  let h = y1 - y0

  targetImg <- targetRegion blockId

  let pointCuts =
        [ PointCut blockId (x0 + xi * w `div` 3, y0 + yi * h `div` 3)
        | xi <- [1..2], yi <- [1..2]
        ]
  let fill = Color blockId (averageColour targetImg)
  let moves = if w >= 50 && h >= 50 then fill : pointCuts else [fill]
  cheapest moves
