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

data Tile = Empty { _ea :: Attributes }
          | Wall  { _wa :: Attributes }
          | Chip  { _ca :: Attributes }

empty = Empty def
wall = Wall def
chip = Chip def
makeLenses ''Tile

image src = translate x y pic
    where pic@(Bitmap w h _ _) = fromJust . unsafePerformIO . loadJuicy $ src
          x = fromIntegral w / 2
          y = fromIntegral h / 2

{-# NOINLINE image #-}

instance MovieClip Tile where
    attrs = lens viewer mutator
      where viewer (Empty a) = a
            viewer (Wall  a) = a
            viewer (Chip  a) = a
            mutator mc new = case mc of
                               Empty _ -> Empty new
                               Wall  _ -> Wall  new
                               Chip  _ -> Chip  new
    render (Empty _) = image "images/empty.png"
    render (Wall _)  = image "images/wall.png"
    render (Chip _)  = image "images/chip.png"

data GameState = GameState {
                    _tiles :: [Tile],
                    _ga :: Attributes
}

makeLenses ''GameState

instance MovieClip GameState where
    attrs = ga
    render gs = pictures $ map ActionKid.display (_tiles gs)

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
