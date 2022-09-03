{-# LANGUAGE OverloadedStrings #-}

module Types (
    module Types
  , PixelRGBA8(..)
  , Word8
 ) where

import Codec.Picture (PixelRGBA8(..), Image, imageHeight,)
import qualified Codec.Picture (pixelAt)
import Data.Word (Word8)
import Data.Map.Strict (Map)
import Data.Aeson
import Data.List (singleton)

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

----------------------------------------------------------------------

data InitialBlock = InitialBlock { iBlockBL :: Point
                                 , iBlockTR :: Point
                                 , iBlockID :: BlockId
                                 , iBlockColor :: RGBA }
  deriving (Show, Eq, Ord)

data InitialLayout = InitialLayout { layoutW :: Int
                                   , layoutH :: Int
                                   , layoutBlocks :: [InitialBlock]
                                   }
  deriving (Show, Eq, Ord)

instance FromJSON PixelRGBA8 where
  parseJSON v = do
    r:g:b:a:[] <- parseJSON v
    pure $ PixelRGBA8 r g b a

instance FromJSON InitialBlock where
  parseJSON = withObject "Block" $
    \o -> InitialBlock <$> o .: "bottomLeft" <*> o .: "topRight" <*> (singleton . read <$> o .: "blockId") <*> o .: "color"

instance FromJSON InitialLayout where
  parseJSON = withObject "BlockMap" $
    \o -> InitialLayout <$> o .: "width" <*> o .: "height" <*> o .: "blocks"

----------------------------------------------------------------------

pixelAt :: Img -> Point -> RGBA
pixelAt img (x, y) = Codec.Picture.pixelAt img x (imageHeight img - y - 1)
