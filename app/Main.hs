{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Foldable ( for_ )
import Data.Monoid (Sum(..))
import Codec.Picture (readImage, Image, PixelRGBA8 (PixelRGBA8), convertRGBA8, imagePixels)

import Types ( ISLLine(Color) )
import ISL (serialize)
import Control.Applicative (Const(..))

main :: IO ()
main = for_ [1..10] $ \i -> do
  img <- load i
  let prog = [Color [0] (average img)]
  putStr $ show i ++ ": " ++ serialize prog

load :: Int -> IO (Image PixelRGBA8)
load i = do
  res <- readImage ("problems/" ++ show i ++ ".png")
  either error (pure . convertRGBA8) res

average :: Image PixelRGBA8 -> PixelRGBA8
average = avg . getConst . imagePixels @PixelRGBA8 @PixelRGBA8 (\(PixelRGBA8 r g b a) -> Const ((s r, s g, s b, s a), Sum 1))
  where
    s = Sum . toInteger
    avg ((Sum r, Sum g, Sum b, Sum a), Sum n) = let f c = fromInteger (c `div` n) in
      PixelRGBA8 (f r) (f g) (f b) (f a)