{-# LANGUAGE LambdaCase #-}
module ISL where

import Types
import Data.List (intercalate)
import Codec.Picture (PixelRGBA8(PixelRGBA8), imageData)
import qualified Data.Vector.Storable as V
import Data.Word (Word8)

testISL :: ISL
testISL = [LineCut [0] X 20, Color [0,0] (PixelRGBA8 255 255 255 255), PointCut [0,1] (30,40)]

-- >>> serialize testISL
-- "cut[0][X][20]\ncolor[0.0][255,255,255,255]\ncut[0.1][30,40]\n"
serialize :: ISL -> String
serialize = unlines . map serializeLine

serializeLine :: ISLLine -> String
serializeLine = \case
  LineCut b o l -> "cut" ++ serializeBlockId b ++ serializeOrientation o ++ serializeLineNumber l
  PointCut b p -> "cut" ++ serializeBlockId b ++ serializePoint p
  Color b c -> "color" ++ serializeBlockId b ++ serializeColor c
  Swap b0 b1 -> "swap" ++ serializeBlockId b0 ++ serializeBlockId b1
  Merge b0 b1 -> "merge" ++ serializeBlockId b0 ++ serializeBlockId b1

serializeBlockId :: BlockId -> String
serializeBlockId = bracket . intercalate "." . map show

serializeLineNumber :: Int -> String
serializeLineNumber = bracket . show

serializePoint :: Point -> String
serializePoint (x, y) = "[" ++ show x ++ "," ++ show y ++ "]"

serializeColor :: RGBA -> String
serializeColor (PixelRGBA8 r g b a) = "[" ++ show r ++ "," ++ show g ++ "," ++ show b ++ "," ++ show a ++ "]"

serializeOrientation :: Orientation -> String
serializeOrientation = bracket . show

bracket :: String -> String
bracket s = "[" ++ s ++ "]"

similarity :: Img -> Img -> Float
similarity a b = 0.05 * go 0 componentDiffs
  where
    go total v | V.length v == 0 = total
    go total v = go (total + sqrt (V.sum (V.take 4 v))) (V.drop 4 v)
    componentDiffs = V.zipWith componentDifference (imageData a) (imageData b)
    componentDifference :: Word8 -> Word8 -> Float
    componentDifference c c' = (fromIntegral c - fromIntegral c') ** 2
