module Types (
    module Types
  , PixelRGBA8(..)
  , Word8
 ) where

import Codec.Picture (PixelRGBA8(..), Image, imageHeight)
import qualified Codec.Picture (pixelAt)
import Data.Word (Word8)

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

data Shape = Rect { bl :: Point, tr :: Point }
  deriving (Show, Eq, Ord)
data Block
  = SimpleBlock Shape
  | ComplexBlock [Block]
  deriving (Show, Eq, Ord)

pixelAt :: Img -> Point -> RGBA
pixelAt img (x, y) = Codec.Picture.pixelAt img x (imageHeight img - y - 1)