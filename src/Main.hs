{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
import ActionKid
import Data.Monoid ((<>), mconcat)
import ActionKid.Utils
import Control.Lens
import Graphics.Gloss
import qualified Debug.Trace as D
import Graphics.Gloss.Juicy
import System.IO.Unsafe
import Data.Maybe
import Codec.Picture hiding (readImage)
import Data.Either
import Codec.Picture.Repa
import qualified Data.Vector.Unboxed as U

data Tile = Empty { _ea :: Attributes }
          | Wall  { _wa :: Attributes }
          | Chip  { _ca :: Attributes }

empty = Empty def
wall  = Wall def
chip  = Chip def

makeLenses ''Tile
deriveMC ''Tile

fromRight (Right x) = x

image :: String -> Picture
image src = translate x y pic
    where pic@(Bitmap w h _ _) = fromJust . unsafePerformIO . loadJuicy $ src
    -- where image = fromRight . unsafePerformIO . readImage $ src
    --       pic@(Bitmap w h _ _) = fromJust . fromDynamicImage . imgToImage $ image
          x = fromIntegral w / 2
          y = fromIntegral h / 2

loadTileMap :: String -> Int -> Int -> [Picture]
loadTileMap src w h = 
    where image = (fromRight . unsafePerformIO . readImage $ src) :: Img RGBA
          vec   = toUnboxed image

-- > :t vec
-- vec :: U.Vector GHC.Word.Word8
--
-- vec U.! 14 => gives you a number (word8)
-- U.length vec == 4096 (4 channels, RGBA, so really 1024...and it's 
-- a 32x32 image. 32x32 = 1024).

{-# NOINLINE image #-}

data GameState = GameState {
                    _tiles :: [Tile],
                    _ga :: Attributes
}

makeLenses ''GameState
deriveMC ''GameState

instance Renderable Tile where
    render (Empty _) = image "images/empty.png"
    render (Wall _)  = image "images/wall.png"
    render (Chip _)  = image "images/chip.png"
    
instance Renderable GameState where
    render gs = displayAll (_tiles gs)

tileMap = 
    [[1, 1, 1, 1],
     [1, 2, 0, 1],
     [1, 0, 0, 1],
     [1, 0, 0, 1],
     [1, 1, 1, 1]]

gameState = GameState tiles def
  where tiles = renderTileMap tileMap f (32,32)
        f 0 = empty
        f 1 = wall
        f 2 = chip

main = run "chips challenge" ((*32) . length . head $ tileMap, (*32) . length $ tileMap) gameState on stepGame

on _ gs = return gs
stepGame _ gs = return gs
