module Types where
import Data.Word (Word8)

type BlockId = [Int]
type Point = (Int, Int)
type RGBA = (Word8, Word8, Word8, Word8)

data Orientation = X | Y
  deriving (Show, Eq, Ord)

data ISL
  = LineCut BlockId Orientation Int
  | PointCut BlockId Point
  | Color BlockId RGBA
  | Swap BlockId BlockId
  | Merge BlockId BlockId
  deriving (Show, Eq, Ord)