module Main where

import Codec.Picture (readImage, convertRGBA8, writePng)
import Data.Foldable ( for_ )
import Data.Maybe (fromMaybe, fromJust)
import Data.Monoid (Sum(..))
import Lens.Micro (over)
import Lens.Micro.Internal (foldMapOf)

import Types
import Cost (cost, similarity)
import ISL (serialize)
import Draw (draw)
import Manually (manually)
import qualified Quads
import Data.Aeson (decodeFileStrict)
import Blocks
import System.Environment (getArgs)

problemNumbersToSolve :: IO [Int]
problemNumbersToSolve = do
  ns <- fmap read <$> getArgs
  pure $ case ns of
    [] -> [1..30] -- TODO: look in solutions dir
    _ -> ns

main :: IO ()
main = problemNumbersToSolve >>= \ns -> for_ ns $ \i -> do
  layout <- loadLayout i
  img <- loadPng i
  let man = lookup i manually
  let prog = fromMaybe (Quads.fromImage img) man
  -- let prog = [Color [0] (average img)]
  let startingBlocks = fromInitialLayout layout
  img' <- draw startingBlocks prog
  let cScore = cost startingBlocks prog
  let sScore = similarity img img'
  save i cScore sScore prog img'
  putStr $ show i ++ " Cost: " ++ show (cScore + sScore, cScore, sScore) ++ "\n" -- ++ serialize prog

loadLayout :: Int -> IO InitialLayout
loadLayout i = fromJust <$> decodeFileStrict ("problems/" ++ show i ++ ".json")

loadPng :: Int -> IO Img
loadPng i = do
  res <- readImage ("problems/" ++ show i ++ ".png")
  either error (pure . convertRGBA8) res

save :: Int -> Integer -> Integer -> ISL -> Img -> IO ()
save i cScore sScore isl img = do
  writeFile ("solutions/" ++ show i ++ ".score") (unlines $ fmap show [cScore + sScore, cScore, sScore])
  writeFile ("solutions/" ++ show i ++ ".isl") (serialize isl)
  writePng ("solutions/" ++ show i ++ ".png") img
