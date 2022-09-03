module Types (
    module Types
  , PixelRGBA8(..)
  , Word8
 ) where

import Codec.Picture (PixelRGBA8(..), Image, imageHeight)
import qualified Codec.Picture (pixelAt)
import Data.Word (Word8)
import Data.Map.Strict (Map)

type BlockId = [Int]
type Point = (Int, Int)
type RGBA = PixelRGBA8
type Img = Image PixelRGBA8

data Orientation = X | Y
  deriving (Show, Eq, Ord)

type ISL = [ISLLine]

data ISLLine
  = LineCut BlockId Orientation Int
  | PointCut BlockId Point
  | Color BlockId RGBA
  | Swap BlockId BlockId
  | Merge BlockId BlockId
  deriving (Show, Eq, Ord)

data Block = Rect { bl :: Point, tr :: Point }
  deriving (Show, Eq, Ord)

type Blocks = (Int, Map BlockId Block)

pixelAt :: Img -> Point -> RGBA
pixelAt img (x, y) = Codec.Picture.pixelAt img x (imageHeight img - y - 1)