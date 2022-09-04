module Swaps where

import Types
import Data.Either (lefts, isLeft)
import Data.List (nub, sortOn)
import Cost
import ImageOps
import qualified Data.Map.Strict as Map
import Debug.Trace
import qualified Blocks

data SwapBlock = SB { blockId :: BlockId, hasColor :: RGBA, wantsColor :: RGBA, improvement :: Integer }

fromImage :: Blocks -> Img -> InitialLayout -> ISL
fromImage blocks img il =
  let options = colors il
      blockIm = im $ head $ Blocks.toList blocks -- assuming all blocks have equal size
      swapCost = lineCost (imgSize img) (Swap [] []) (imgSize blockIm)
      annotated
        = sortOn (negate . improvement)
        $ fmap (\(blockId, block) -> uncurry (SB blockId (pixelAt (im block) (0, 0))) $ bestColor img options block)
        $ Map.toList
        $ snd blocks
  in findSwaps swapCost annotated []

findSwaps :: Integer -> [SwapBlock] -> ISL -> ISL
findSwaps _ [] isl = isl
findSwaps swapCost (sb:sbs) isl =
  case findElem (\sb' -> hasColor sb' == wantsColor sb && hasColor sb == wantsColor sb') sbs of
    Just (sb', sbs') ->
      if improvement sb + improvement sb' > swapCost
        then findSwaps swapCost sbs' (isl ++ [Swap (blockId sb) (blockId sb')])
        else isl
    Nothing -> findSwaps swapCost sbs isl

colors :: InitialLayout -> [RGBA]
colors il = nub $ lefts $ map iBlockContents $ layoutBlocks il

-- best color with cost improvement
bestColor :: Img -> [RGBA] -> Block -> (RGBA, Integer)
bestColor img options (Rect p0 p1 bim) =
  let blockImg = region img p0 p1
      doNothingCost = similarity blockImg bim
  in head $ sortOn (negate . snd) $ map (\c -> (c, doNothingCost - similarityWithSolidColor blockImg c)) options


findElem       :: (a -> Bool) -> [a] -> Maybe (a, [a])
findElem p     = find' id
    where
      find' _ []         = Nothing
      find' prefix (x : xs)
          | p x          = Just (x, prefix xs)
          | otherwise    = find' (prefix . (x:)) xs
